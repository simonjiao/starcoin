// Copyright (c) The Starcoin Core Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::Result;
use bcs_ext::BCSCodec;
use serde::{Deserialize, Serialize};
use starcoin_crypto::HashValue;
use starcoin_schemadb::{
    db::{
        TRANSACTION_INFO_HASH_PREFIX_NAME, TRANSACTION_INFO_PREFIX_NAME,
        TRANSACTION_INFO_PREFIX_NAME_V2,
    },
    define_schema,
    schema::{KeyCodec, ValueCodec},
};
use starcoin_types::transaction::{RichTransactionInfo, TransactionInfo as TxnInfo};
use starcoin_vm_types::vm_status::KeptVMStatus;

define_schema!(
    TransactionInfo,
    HashValue,
    RichTransactionInfo,
    TRANSACTION_INFO_PREFIX_NAME_V2
);

impl KeyCodec<TransactionInfo> for HashValue {
    fn encode_key(&self) -> Result<Vec<u8>> {
        self.encode()
    }
    fn decode_key(data: &[u8]) -> Result<Self> {
        <Self as BCSCodec>::decode(data)
    }
}

impl ValueCodec<TransactionInfo> for RichTransactionInfo {
    fn encode_value(&self) -> Result<Vec<u8>> {
        self.encode()
    }
    fn decode_value(data: &[u8]) -> Result<Self> {
        <Self as BCSCodec>::decode(data)
    }
}

define_schema!(
    TransactionInfoHash,
    HashValue,
    Vec<HashValue>,
    TRANSACTION_INFO_HASH_PREFIX_NAME
);

impl KeyCodec<TransactionInfoHash> for HashValue {
    fn encode_key(&self) -> Result<Vec<u8>> {
        self.encode()
    }

    fn decode_key(data: &[u8]) -> Result<Self> {
        <Self as BCSCodec>::decode(data)
    }
}

impl ValueCodec<TransactionInfoHash> for Vec<HashValue> {
    fn encode_value(&self) -> Result<Vec<u8>> {
        self.encode()
    }
    fn decode_value(data: &[u8]) -> Result<Self> {
        <Self as BCSCodec>::decode(data)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct BlockTransactionInfo {
    pub block_id: HashValue,
    pub txn_info: TxnInfo,
}

impl Default for BlockTransactionInfo {
    fn default() -> Self {
        Self {
            block_id: Default::default(),
            txn_info: TxnInfo {
                transaction_hash: Default::default(),
                state_root_hash: Default::default(),
                event_root_hash: Default::default(),
                gas_used: 0,
                status: KeptVMStatus::Executed,
            },
        }
    }
}

// This column family is deprecated
define_schema!(
    OldTransactionInfo,
    HashValue,
    BlockTransactionInfo,
    TRANSACTION_INFO_PREFIX_NAME
);

impl KeyCodec<OldTransactionInfo> for HashValue {
    fn encode_key(&self) -> Result<Vec<u8>> {
        self.encode()
    }

    fn decode_key(data: &[u8]) -> Result<Self> {
        <Self as BCSCodec>::decode(data)
    }
}

impl ValueCodec<OldTransactionInfo> for BlockTransactionInfo {
    fn encode_value(&self) -> Result<Vec<u8>> {
        self.encode()
    }

    fn decode_value(data: &[u8]) -> Result<Self> {
        <Self as BCSCodec>::decode(data)
    }
}
