use crate::block::{Block, BlockHeader};
use crate::transaction::SignedUserTransaction;
use bcs_ext::Sample;
use serde::{Deserialize, Serialize};
use starcoin_crypto::HashValue;

#[derive(Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct CompactBlock {
    pub header: BlockHeader,
    pub short_ids: Vec<ShortId>,
    pub prefilled_txn: Vec<PrefilledTxn>,
    pub uncles: Option<Vec<BlockHeader>>,
    pub tips: Option<Vec<HashValue>>,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct PrefilledTxn {
    pub index: u64,
    pub tx: SignedUserTransaction,
}

// TODO: change to siphash24 of 6bites
#[derive(Clone, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct ShortId(pub HashValue);

impl CompactBlock {
    pub fn new(block: Block, tips: Option<Vec<HashValue>>) -> Self {
        let prefilled_txn: Vec<PrefilledTxn> = vec![];
        let header = block.header;
        let short_ids: Vec<ShortId> = block
            .body
            .transactions
            .into_iter()
            .map(|tx| tx.id())
            .map(ShortId)
            .collect();
        CompactBlock {
            header,
            short_ids,
            prefilled_txn,
            uncles: block.body.uncles,
            tips,
        }
    }

    pub fn txn_len(&self) -> usize {
        self.short_ids.len()
    }

    pub fn dag_parent_and_tips(&self) -> Option<(&BlockHeader, &[BlockHeader])> {
        self.uncles.as_ref().and_then(|uncles| uncles.split_first())
    }
}

// impl From<Block> for CompactBlock {
//     fn from(block: Block) -> Self {
//         CompactBlock::new(block)
//     }
// }

impl Sample for CompactBlock {
    fn sample() -> Self {
        // Block::sample().into()
        Self::new(Block::sample(), None)
    }
}
