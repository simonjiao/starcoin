// Copyright (c) The Starcoin Core Contributors
// SPDX-License-Identifier: Apache-2.0s

use crate::node::ACCUMULATOR_PLACEHOLDER_HASH;
use crate::node_index::{FrozenSubTreeIterator, NODE_ERROR_INDEX};
use crate::node_index::{NodeIndex, MAX_ACCUMULATOR_PROOF_DEPTH};
use crate::tree_store::AccumulatorCache;
use crate::{AccumulatorNode, AccumulatorTreeStore, LeafCount, NodeCount};
use anyhow::Result;
use logger::prelude::*;
use mirai_annotations::*;
use starcoin_crypto::HashValue;
use std::cell::RefCell;
use std::sync::Arc;

pub struct AccumulatorTree {
    /// Accumulator id
    id: HashValue,
    /// forzen subtree roots hashes.
    frozen_subtree_roots: RefCell<Vec<HashValue>>,
    /// The total number of leaves in this accumulator.
    pub(crate) num_leaves: LeafCount,
    /// The total number of nodes in this accumulator.
    pub(crate) num_nodes: NodeCount,
    /// The root hash of this accumulator.
    pub(crate) root_hash: HashValue,
    /// The storage of accumulator.
    store: Arc<dyn AccumulatorTreeStore>,
}

impl AccumulatorTree {
    pub fn new(
        accumulator_id: HashValue,
        frozen_subtree_roots: Vec<HashValue>,
        num_leaves: LeafCount,
        num_nodes: NodeCount,
        root_hash: HashValue,
        store: Arc<dyn AccumulatorTreeStore>,
    ) -> Self {
        trace!("accumulator cache new: {:?}", accumulator_id.short_str());
        Self {
            id: accumulator_id,
            frozen_subtree_roots: RefCell::new(frozen_subtree_roots),
            num_leaves,
            num_nodes,
            root_hash,
            store,
        }
    }

    ///append from multiple leaves
    pub(crate) fn append_leaves(
        &mut self,
        new_leaves: &[HashValue],
    ) -> Result<(HashValue, Vec<AccumulatorNode>)> {
        // Deal with the case where new_leaves is empty
        if new_leaves.is_empty() {
            if self.num_leaves == 0 {
                return Ok((*ACCUMULATOR_PLACEHOLDER_HASH, Vec::new()));
            } else {
                return Ok((self.root_hash, Vec::new()));
            }
        }

        let num_new_leaves = new_leaves.len();
        let last_new_leaf_count = self.num_leaves + num_new_leaves as LeafCount;
        let mut new_num_nodes = self.num_nodes;
        let root_level = NodeIndex::root_level_from_leaf_count(last_new_leaf_count);
        let mut to_freeze = Vec::with_capacity(Self::max_to_freeze(num_new_leaves, root_level));
        // Iterate over the new leaves, adding them to to_freeze and then adding any frozen parents
        // when right children are encountered.  This has the effect of creating frozen nodes in
        // perfect post-order, which can be used as a strictly increasing append only index for
        // the underlying storage.
        //
        // We will track newly created left siblings while iterating so we can pair them with their
        // right sibling, if and when it becomes frozen.  If the frozen left sibling is not created
        // in this iteration, it must already exist in storage.
        let mut left_siblings: Vec<(_, _)> = Vec::new();
        for (leaf_offset, leaf) in new_leaves.iter().enumerate() {
            let leaf_pos = NodeIndex::from_leaf_index(self.num_leaves + leaf_offset as LeafCount);
            let mut hash = *leaf;
            to_freeze.push(AccumulatorNode::new_leaf(leaf_pos, hash));
            debug!(
                "{:?} insert leaf cache: {:?}",
                self.id.short_str(),
                leaf_pos
            );
            new_num_nodes += 1;
            let mut pos = leaf_pos;
            while pos.is_right_child() {
                let mut internal_node = AccumulatorNode::Empty;
                let sibling = pos.sibling();

                hash = match left_siblings.pop() {
                    Some((x, left_hash)) => {
                        assert_eq!(x, sibling);
                        internal_node =
                            AccumulatorNode::new_internal(pos.parent(), left_hash, hash);
                        internal_node.hash()
                    }
                    None => {
                        internal_node = AccumulatorNode::new_internal(
                            pos.parent(),
                            self.get_node_hash(sibling).unwrap(),
                            hash,
                        );
                        internal_node.hash()
                    }
                };
                pos = pos.parent();
                to_freeze.push(internal_node);
                new_num_nodes += 1;
            }
            // The node remaining must be a left child, possibly the root of a complete binary tree.
            left_siblings.push((pos, hash));
        }

        // Now reconstruct the final root hash by walking up to root level and adding
        // placeholder hash nodes as needed on the right, and left siblings that have either
        // been newly created or read from storage.
        let (mut pos, mut hash) = left_siblings.pop().expect("Must have at least one node");
        for _ in pos.level()..root_level as u32 {
            hash = if pos.is_left_child() {
                AccumulatorNode::new_internal(pos.parent(), hash, *ACCUMULATOR_PLACEHOLDER_HASH)
                    .hash()
            } else {
                let sibling = pos.sibling();
                match left_siblings.pop() {
                    Some((x, left_hash)) => {
                        assert_eq!(x, sibling);
                        AccumulatorNode::new_internal(pos.parent(), left_hash, hash).hash()
                    }
                    None => AccumulatorNode::new_internal(
                        pos.parent(),
                        self.get_node_hash(sibling).unwrap(),
                        hash,
                    )
                    .hash(),
                }
            };
            pos = pos.parent();
        }
        assert!(left_siblings.is_empty());

        // udpate to cache
        self.update_cache(to_freeze.clone());
        // update self properties
        self.root_hash = hash;
        self.frozen_subtree_roots = RefCell::new(
            FrozenSubTreeIterator::new(last_new_leaf_count)
                .map(|p| self.get_node_hash(p).unwrap())
                .collect::<Vec<_>>(),
        );
        self.num_leaves = last_new_leaf_count;
        self.num_nodes = new_num_nodes;

        Ok((hash, to_freeze))
    }

    /// Get accumulator node by hash.
    fn get_node(&self, node_hash: HashValue) -> AccumulatorNode {
        match self.store.clone().get_node(node_hash) {
            Ok(Some(node)) => node,
            _ => {
                error!("get accumulator node err:{:?}", node_hash);
                AccumulatorNode::new_empty()
            }
        }
    }

    /// Computes the root hash of an accumulator given the frozen subtree roots and the number of
    /// leaves in this accumulator.
    fn compute_root_hash(frozen_subtree_roots: &[HashValue], num_leaves: LeafCount) -> HashValue {
        match frozen_subtree_roots.len() {
            0 => return *ACCUMULATOR_PLACEHOLDER_HASH,
            1 => return frozen_subtree_roots[0],
            _ => (),
        }

        // The trailing zeros do not matter since anything below the lowest frozen subtree is
        // already represented by the subtree roots.
        let mut bitmap = num_leaves >> num_leaves.trailing_zeros();
        let mut current_hash = *ACCUMULATOR_PLACEHOLDER_HASH;
        let mut frozen_subtree_iter = frozen_subtree_roots.iter().rev();

        while bitmap > 0 {
            current_hash = if bitmap & 1 != 0 {
                AccumulatorNode::new_internal(
                    NODE_ERROR_INDEX.to_owned(),
                    *frozen_subtree_iter
                        .next()
                        .expect("This frozen subtree should exist."),
                    current_hash,
                )
            } else {
                AccumulatorNode::new_internal(
                    NODE_ERROR_INDEX.to_owned(),
                    current_hash,
                    *ACCUMULATOR_PLACEHOLDER_HASH,
                )
            }
            .hash();
            bitmap >>= 1;
        }

        current_hash
    }

    pub(crate) fn get_frozen_subtree_roots(&self) -> Result<Vec<HashValue>> {
        let result = FrozenSubTreeIterator::new(self.num_leaves)
            .map(|p| self.get_node_hash(p).unwrap())
            .collect::<Vec<_>>();
        Ok(result)
    }

    /// filter function can be applied to filter out certain siblings.
    pub(crate) fn get_siblings(
        &self,
        leaf_index: u64,
        filter: impl Fn(NodeIndex) -> bool,
    ) -> Result<Vec<HashValue>> {
        let root_pos = NodeIndex::root_from_leaf_count(self.num_leaves);
        let siblings = NodeIndex::from_leaf_index(leaf_index)
            .iter_ancestor_sibling()
            .take(root_pos.level() as usize)
            .filter_map(|p| {
                if filter(p) {
                    Some(self.get_node_hash(p))
                } else {
                    None
                }
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(siblings)
    }

    /// Get node hash by index.
    pub(crate) fn get_node_hash(&self, node_index: NodeIndex) -> Result<HashValue> {
        let idx = self.rightmost_leaf_index();
        if node_index.is_placeholder(idx) {
            Ok(*ACCUMULATOR_PLACEHOLDER_HASH)
        } else {
            let node_hash = AccumulatorCache::get_node_hash(self.id, node_index);
            if node_hash == HashValue::zero() {
                //TODO get from storage
            }
            Ok(node_hash)
        }
    }

    /// update node to cache
    fn update_cache(&self, node_vec: Vec<AccumulatorNode>) -> Result<()> {
        info!("accumulator update cache.");
        AccumulatorCache::save_nodes(node_vec.clone());
        AccumulatorCache::save_node_indexes(self.id, node_vec)
    }

    // ///get new root by leaf index and update
    // fn get_new_root_and_update_node(
    //     &self,
    //     leaf_index: NodeIndex,
    //     root_index: NodeIndex,
    // ) -> Result<HashValue> {
    //     let mut right_hash = *ACCUMULATOR_PLACEHOLDER_HASH;
    //     let mut right_index = leaf_index.clone();
    //     #[allow(unused_assignments)]
    //     let mut new_root = right_hash;
    //     loop {
    //         //get sibling
    //         let sibling_index = right_index.sibling();
    //         if sibling_index.to_inorder_index() > leaf_index.to_inorder_index() {
    //             //right left replace node
    //             let left_hash = right_hash;
    //             right_hash = *ACCUMULATOR_PLACEHOLDER_HASH;
    //             let parent_index = right_index.parent();
    //             //set new root hash to parent node hash
    //             let parent_node =
    //                 AccumulatorNode::new_internal(parent_index, left_hash, right_hash);
    //             new_root = parent_node.hash();
    //             self.update_node(parent_index, new_root, parent_node.clone())?;
    //             if parent_index == root_index {
    //                 //get root node
    //                 break;
    //             }
    //             //for next loop
    //             right_index = parent_node.index();
    //             right_hash = new_root;
    //         } else {
    //             let sibling_hash = self.get_index(sibling_index).unwrap();
    //             match self.node_store.get_node(sibling_hash) {
    //                 Ok(Some(node)) => {
    //                     let left_hash = node.hash();
    //                     let parent_index = right_index.parent();
    //                     //set new root hash to parent node hash
    //                     let parent_node =
    //                         AccumulatorNode::new_internal(parent_index, left_hash, right_hash);
    //                     new_root = parent_node.hash();
    //                     self.update_node(parent_index, new_root, parent_node.clone())?;
    //                     if parent_index == root_index {
    //                         //get root node
    //                         break;
    //                     }
    //                     //for next loop
    //                     right_index = parent_node.index();
    //                     right_hash = new_root;
    //                 }
    //                 _ => {
    //                     warn!("get leaf node error: {:?}", sibling_index);
    //                 }
    //             }
    //         }
    //     }
    //     Ok(new_root)
    // }
    /// Update node storage,and index cache
    // fn update_node(&self, index: NodeIndex, hash: HashValue, node: AccumulatorNode) -> Result<()> {
    //     self.node_store.save_node(node.clone())?;
    //     self.index_cache.borrow_mut().insert(index, hash);
    //     Ok(())
    // }

    fn rightmost_leaf_index(&self) -> u64 {
        if self.num_leaves == 0 {
            0 as u64
        } else {
            (self.num_leaves - 1) as u64
        }
    }

    /// upper bound of num of frozen nodes:
    ///     new leaves and resulting frozen internal nodes forming a complete binary subtree
    ///         num_new_leaves * 2 - 1 < num_new_leaves * 2
    ///     and the full route from root of that subtree to the accumulator root turns frozen
    ///         height - (log2(num_new_leaves) + 1) < height - 1 = root_level
    fn max_to_freeze(num_new_leaves: usize, root_level: u32) -> usize {
        precondition!(root_level as usize <= MAX_ACCUMULATOR_PROOF_DEPTH);
        precondition!(num_new_leaves < (usize::max_value() / 2));
        precondition!(num_new_leaves * 2 <= usize::max_value() - root_level as usize);
        num_new_leaves * 2 + root_level as usize
    }
}
