use std::sync::Arc;

use anyhow::bail;
use dag_consensus::blockdag::BlockDAG;
use dag_database::prelude::{FlexiDagStorage, FlexiDagStorageConfig};
use starcoin_accumulator::Accumulator;
use starcoin_accumulator::{node::AccumulatorStoreType, MerkleAccumulator};
use starcoin_config::NodeConfig;
use starcoin_crypto::HashValue;
use starcoin_executor::VMMetrics;
use starcoin_storage::storage::CodecKVStore;
use starcoin_storage::{flexi_dag::SyncFlexiDagSnapshotStorage, Store};
use starcoin_types::block::BlockHeader;
use starcoin_types::{blockhash::ORIGIN, header::Header};
use starcoin_network_rpc_api::dag_protocol::{TargetDagAccumulatorLeaf, GetDagAccumulatorLeaves};

pub struct DagBlockChain {
    dag: Option<BlockDAG>,
    dag_sync_accumulator: MerkleAccumulator,
    dag_sync_accumulator_snapshot: Arc<SyncFlexiDagSnapshotStorage>,
}

impl DagBlockChain {
    pub fn new(
        config: Arc<NodeConfig>,
        storage: Arc<dyn Store>,
        vm_metrics: Option<VMMetrics>,
    ) -> anyhow::Result<Self> {
        // initialize the dag
        let db_path = config.storage.dir();
        let config = FlexiDagStorageConfig::create_with_params(1, 0, 1024);
        let db = FlexiDagStorage::create_from_path(db_path, config)?;
        let dag = BlockDAG::new(
            Header::new(BlockHeader::random(), vec![HashValue::new(ORIGIN)]),
            16,
            db,
        );

        // initialize the block accumulator
        let startup_info = match storage.get_flexi_dag_startup_info()? {
            Some(startup_info) => startup_info,
            None => {
                return Ok(Self {
                    dag: Some(dag),
                    dag_sync_accumulator: MerkleAccumulator::new_empty(
                        storage.get_accumulator_store(AccumulatorStoreType::SyncDag),
                    ),
                    dag_sync_accumulator_snapshot: storage.get_accumulator_snapshot_storage(),
                })
            }
        };

        // let accmulator_info = sync_flexi_dag_store.get_snapshot_storage().get(startup_info.main);
        let accumulator_info = match storage.query_by_hash(startup_info.main) {
            Ok(op_snapshot) => match op_snapshot {
                Some(snapshot) => snapshot.accumulator_info,
                None => bail!("failed to get sync accumulator info since it is None"),
            },
            Err(error) => bail!("failed to get sync accumulator info: {}", error.to_string()),
        };

        Ok(Self {
            dag: Some(dag),
            dag_sync_accumulator: MerkleAccumulator::new_with_info(
                accumulator_info,
                storage.get_accumulator_store(AccumulatorStoreType::SyncDag),
            ),
            dag_sync_accumulator_snapshot: storage.get_accumulator_snapshot_storage(),
        })
    }

    pub fn get_accumulator_leaves(&self, req: GetDagAccumulatorLeaves) -> anyhow::Result<Vec<TargetDagAccumulatorLeaf>> {
        match self.dag_sync_accumulator.get_leaves(req.accumulator_leaf_index, true, req.batch_size) {
            Ok(leaves) => Ok(leaves
                .into_iter()
                .enumerate()
                .map(
                    |(index, leaf)| match self.dag_sync_accumulator_snapshot.get(leaf) {
                        Ok(op_snapshot) => {
                            let snapshot = op_snapshot.expect("snapshot must exist");
                            TargetDagAccumulatorLeaf {
                                accumulator_root: snapshot.accumulator_info.accumulator_root,
                                leaf_index: req.accumulator_leaf_index.saturating_sub(index as u64),
                            }
                        }
                        Err(error) => {
                            panic!(
                                "error occured when query the accumulator snapshot: {}",
                                error.to_string()
                            );
                        }
                    },
                )
                .collect()),
            Err(error) => {
                bail!(
                    "an error occured when getting the leaves of the accumulator, {}",
                    error.to_string()
                );
            }
        }
    }
}
