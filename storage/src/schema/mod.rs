mod transaction;
mod transaction_info;

use crate::cache_storage::GCacheStorage;
use crate::schema::transaction_info::OldTransactionInfo;
use anyhow::Result;
use starcoin_schemadb::iterator::SchemaIterator;
use starcoin_schemadb::schema::Schema;
pub(crate) use {
    transaction::Transaction,
    transaction_info::{TransactionInfo, TransactionInfoHash},
};

pub(crate) type TransactionStorage = TransactionStore<Transaction>;
pub(crate) type TransactionInfoStorage = TransactionStore<TransactionInfo>;
pub(crate) type TransactionInfoHashStorage = TransactionStore<TransactionInfoHash>;
pub(crate) type OldTransactionInfoStorage = TransactionStore<OldTransactionInfo>;

#[derive(Clone)]
pub(crate) struct TransactionStore<S: Schema> {
    cache: std::sync::Arc<GCacheStorage<S::Key, S::Value>>,
}

impl<S: Schema> TransactionStore<S> {
    pub fn new() -> Self {
        Self {
            cache: std::sync::Arc::new(GCacheStorage::new(None)),
        }
    }

    pub fn get_item(&self, key: &S::Key) -> Result<Option<S::Value>> {
        Ok(self.cache.get_inner(key))
    }

    pub fn get_items(&self, keys: &[S::Key]) -> Result<Vec<Option<S::Value>>> {
        Ok(self.cache.multi_get_inner(keys))
    }

    pub fn save_item(&self, key: S::Key, value: S::Value) -> Result<()> {
        self.cache.put_inner(key, value);
        Ok(())
    }

    pub fn save_items(&self, items: impl Iterator<Item = (S::Key, S::Value)>) -> Result<()> {
        items.for_each(|(k, v)| {
            self.cache.put_inner(k, v);
        });
        Ok(())
    }
}
