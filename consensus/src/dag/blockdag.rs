use super::ghostdag::protocol::{ColoringOutput, GhostdagManager};
use super::reachability::{inquirer, reachability_service::MTReachabilityService};
use super::types::ghostdata::GhostdagData;
use crate::consensusdb::prelude::StoreError;
use crate::consensusdb::schemadb::GhostdagStoreReader;
use crate::consensusdb::{
    prelude::FlexiDagStorage,
    schemadb::{
        DbGhostdagStore, DbHeadersStore, DbReachabilityStore, DbRelationsStore, GhostdagStore,
        HeaderStore, ReachabilityStoreReader, RelationsStore, RelationsStoreReader,
    },
};
use anyhow::{anyhow, bail, Ok};
use parking_lot::RwLock;
use starcoin_crypto::{HashValue as Hash, HashValue};
use starcoin_types::block::BlockHeader;
use starcoin_types::{
    blockhash::{BlockHashes, KType, ORIGIN},
    consensus_header::ConsensusHeader,
};
use std::sync::Arc;

pub type DbGhostdagManager = GhostdagManager<
    DbGhostdagStore,
    DbRelationsStore,
    MTReachabilityService<DbReachabilityStore>,
    DbHeadersStore,
>;

#[derive(Clone)]
pub struct BlockDAG {
    storage: FlexiDagStorage,
    ghostdag_manager: DbGhostdagManager,
}

impl BlockDAG {
    pub fn new(k: KType, db: FlexiDagStorage) -> Self {
        let ghostdag_store = db.ghost_dag_store.clone();
        let header_store = db.header_store.clone();
        let relations_store = db.relations_store.clone();
        let mut reachability_store = db.reachability_store.clone();
        inquirer::init(&mut reachability_store).unwrap();
        let reachability_service =
            MTReachabilityService::new(Arc::new(RwLock::new(reachability_store)));

        let ghostdag_manager = DbGhostdagManager::new(
            k,
            ghostdag_store.clone(),
            relations_store.clone(),
            header_store.clone(),
            reachability_service,
        );

        let mut dag = Self {
            ghostdag_manager,
            storage: db,
        };
        dag
    }

    pub fn init_with_genesis(&self, genesis: BlockHeader) -> anyhow::Result<()> {
        if self.storage.relations_store.has(Hash::new(ORIGIN))? {
            return Err(anyhow!("Already init with genesis"));
        };
        self.storage
            .relations_store
            .insert(Hash::new(ORIGIN), BlockHashes::new(vec![]))
            .unwrap();
        let _ = self.commit(genesis);
        Ok(())
    }
    pub fn ghostdata(&self, parents: &[HashValue]) -> GhostdagData {
        self.ghostdag_manager.ghostdag(parents)
    }

    pub fn commit(&self, header: BlockHeader) -> anyhow::Result<()> {
        // Generate ghostdag data
        let parents_hash = header.parents();

        let ghostdag_data = if !header.is_dag_genesis() {
            self.ghostdag_manager.ghostdag(parents_hash.as_slice())
        } else {
            self.ghostdag_manager.genesis_ghostdag_data()
        };
        // Store ghostdata
        self.storage
            .ghost_dag_store
            .insert(header.id(), Arc::new(ghostdag_data.clone()))
            .unwrap();

        // Update reachability store
        let mut reachability_store = self.storage.reachability_store.clone();
        let mut merge_set = ghostdag_data
            .unordered_mergeset_without_selected_parent()
            .filter(|hash| self.storage.reachability_store.has(*hash).unwrap());

        inquirer::add_block(
            &mut reachability_store,
            header.id(),
            ghostdag_data.selected_parent,
            &mut merge_set,
        )?;

        // store relations
        self.storage
            .relations_store
            .insert(header.id(), BlockHashes::new(parents_hash.to_vec()))?;
        // Store header store
        let _ = self
            .storage
            .header_store
            .insert(header.id(), Arc::new(header.to_owned()), 0)?;
        return Ok(());
    }

    pub fn get_parents(&self, hash: Hash) -> anyhow::Result<Vec<Hash>> {
        match self.storage.relations_store.get_parents(hash) {
            anyhow::Result::Ok(parents) => anyhow::Result::Ok((*parents).clone()),
            Err(error) => {
                println!("failed to get parents by hash: {}", error.to_string());
                bail!("failed to get parents by hash: {}", error.to_string());
            }
        }
    }

    pub fn get_children(&self, hash: Hash) -> anyhow::Result<Vec<Hash>> {
        match self.storage.relations_store.get_children(hash) {
            anyhow::Result::Ok(children) => anyhow::Result::Ok((*children).clone()),
            Err(error) => {
                println!("failed to get parents by hash: {}", error.to_string());
                bail!("failed to get parents by hash: {}", error.to_string());
            }
        }
    }

    // for testing
    pub fn push_parent_children(
        &mut self,
        child: Hash,
        parents: Arc<Vec<Hash>>,
    ) -> Result<(), StoreError> {
        self.storage.relations_store.insert(child, parents)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::consensusdb::prelude::{FlexiDagStorage, FlexiDagStorageConfig};
    use starcoin_types::block::{BlockHeader, BlockTemplate};
    use std::{env, fs};

    #[test]
    fn base_test() {
        let genesis = BlockHeader::dag_genesis_random();
        let genesis_hash = genesis.hash();
        let k = 16;
        let db_path = env::temp_dir().join("smolstc");
        println!("db path:{}", db_path.to_string_lossy());
        if db_path
            .as_path()
            .try_exists()
            .unwrap_or_else(|_| panic!("Failed to check {db_path:?}"))
        {
            fs::remove_dir_all(db_path.as_path()).expect("Failed to delete temporary directory");
        }
        let config = FlexiDagStorageConfig::create_with_params(1, 0, 1024);
        let db = FlexiDagStorage::create_from_path(db_path, config)
            .expect("Failed to create flexidag storage");
        let mut dag = BlockDAG::new(k, db);
        dag.init_with_genesis(genesis);
        let mut block = BlockHeader::random();
        block.set_parents(vec![genesis_hash]);
        dag.commit(block);
    }
}