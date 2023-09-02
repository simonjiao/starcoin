// Copyright (c) The Starcoin Core Contributors
// SPDX-License-Identifier: Apache-2.0

mod schema;

pub use schema::*;

/*
use crate::storage::CodecWriteBatch;
use anyhow::{Error, Result};
use starcoin_crypto::HashValue;
use starcoin_types::transaction::RichTransactionInfo;

impl TransactionInfoHashStorage {
    pub(crate) fn get_transaction_info_ids_by_hash(
        &self,
        txn_hash: HashValue,
    ) -> Result<Vec<HashValue>, Error> {
        if let Some(txn_id_vec) = self.get(txn_hash)? {
            Ok(txn_id_vec)
        } else {
            Ok(vec![])
        }
    }

    pub(crate) fn save_transaction_infos(
        &self,
        vec_txn_info: &[RichTransactionInfo],
    ) -> Result<(), Error> {
        let mut batch = CodecWriteBatch::new();
        for txn_info in vec_txn_info {
            if let Some(mut id_vec) = self.get(txn_info.transaction_hash())? {
                if !id_vec.contains(&txn_info.id()) {
                    id_vec.push(txn_info.id());
                    batch.put(txn_info.transaction_hash(), id_vec)?;
                }
            } else {
                batch.put(txn_info.transaction_hash(), vec![txn_info.id()])?;
            }
        }
        self.write_batch(batch)
    }
}
impl TransactionInfoStorage {
    pub(crate) fn get_transaction_info(
        &self,
        id: HashValue,
    ) -> Result<Option<RichTransactionInfo>, Error> {
        self.get(id)
    }
    pub(crate) fn save_transaction_infos(
        &self,
        vec_txn_info: Vec<RichTransactionInfo>,
    ) -> Result<(), Error> {
        let mut batch = CodecWriteBatch::new();
        for txn_info in vec_txn_info {
            batch.put(txn_info.id(), txn_info)?;
        }
        self.write_batch(batch)
    }

    pub(crate) fn get_transaction_infos(
        &self,
        ids: Vec<HashValue>,
    ) -> Result<Vec<Option<RichTransactionInfo>>, Error> {
        self.multiple_get(ids)
    }
}
*/
