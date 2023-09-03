// Copyright (c) The Starcoin Core Contributors
// SPDX-License-Identifier: Apache-2.0

use criterion::{BatchSize, Bencher};
use starcoin_crypto::HashValue;
use starcoin_schemadb::SchemaBatch;
use starcoin_storage::BlockTransactionInfoStore;
use starcoin_storage::Storage;
use starcoin_types::transaction::TransactionInfo;
use starcoin_vm_types::transaction::RichTransactionInfo;
use starcoin_vm_types::vm_status::KeptVMStatus;

/// Benchmarking support for storage.
pub struct StorageBencher {
    storage: Storage,
}

/// The number of accounts created by default.
pub const DEFAULT_NUM_ACCOUNTS: usize = 100;

/// The number of transactions created by default.
pub const DEFAULT_NUM_TRANSACTIONS: usize = 200;

impl StorageBencher {
    fn setup() -> Vec<RichTransactionInfo> {
        let mut txn_infos = Vec::with_capacity(DEFAULT_NUM_TRANSACTIONS);
        for _i in 0..DEFAULT_NUM_TRANSACTIONS {
            let transaction_info1 = TransactionInfo::new(
                HashValue::random(),
                HashValue::zero(),
                vec![].as_slice(),
                0,
                KeptVMStatus::Executed,
            );
            let ri = RichTransactionInfo::new(
                HashValue::zero(),
                rand::random(),
                transaction_info1,
                rand::random(),
                rand::random(),
            );
            txn_infos.push(ri);
        }
        txn_infos
    }

    /// Creates a new transaction bencher with default settings.
    pub fn new(storage: Storage) -> Self {
        Self { storage }
    }

    /// Executes this state in a single block.
    fn execute(&self, txn_infos: Vec<RichTransactionInfo>) {
        let batch = SchemaBatch::new();
        self.storage
            .save_transaction_infos(txn_infos, &batch)
            .unwrap();
        self.storage.ledger_db().write_schemas(batch).unwrap();
    }
    /// Runs the bencher.
    pub fn bench(&self, b: &mut Bencher) {
        b.iter_batched(
            || (self, Self::setup()),
            |(bench, txn_infos)| bench.execute(txn_infos),
            BatchSize::LargeInput,
        )
    }
}
