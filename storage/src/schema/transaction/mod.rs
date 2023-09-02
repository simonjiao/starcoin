// Copyright (c) The Starcoin Core Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::Result;
use bcs_ext::BCSCodec;
use starcoin_crypto::HashValue;
use starcoin_schemadb::{
    db::TRANSACTION_PREFIX_NAME,
    define_schema,
    schema::{KeyCodec, ValueCodec},
};
use starcoin_types::transaction::Transaction as TxnType;

define_schema!(Transaction, HashValue, TxnType, TRANSACTION_PREFIX_NAME);

impl KeyCodec<Transaction> for HashValue {
    fn encode_key(&self) -> Result<Vec<u8>> {
        self.encode()
    }
    fn decode_key(data: &[u8]) -> Result<Self> {
        Self::decode(data)
    }
}

impl ValueCodec<Transaction> for TxnType {
    fn encode_value(&self) -> Result<Vec<u8>> {
        self.encode()
    }

    fn decode_value(data: &[u8]) -> Result<Self> {
        Self::decode(data)
    }
}
/*
impl TransactionStore for TransactionStorage {
    fn get_transaction(&self, txn_hash: HashValue) -> Result<Option<Transaction>> {
        self.get(txn_hash)
    }

    fn save_transaction(&self, txn_info: Transaction) -> Result<()> {
        self.put(txn_info.id(), txn_info)
    }

    fn save_transaction_batch(&self, txn_vec: Vec<Transaction>) -> Result<()> {
        let batch =
            CodecWriteBatch::new_puts(txn_vec.into_iter().map(|txn| (txn.id(), txn)).collect());
        self.write_batch(batch)
    }

    fn get_transactions(&self, txn_hash_vec: Vec<HashValue>) -> Result<Vec<Option<Transaction>>> {
        self.multiple_get(txn_hash_vec)
    }
}
*/

#[cfg(test)]
mod test;
