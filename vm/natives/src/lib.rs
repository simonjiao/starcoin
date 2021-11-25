// Copyright (c) The Starcoin Core Contributors
// SPDX-License-Identifier: Apache-2.0

pub mod account;
pub mod bcs;
pub mod debug;
pub mod hash;
pub mod signature;
pub mod token;
pub mod u256;
// for support evm compat and cross chain.
pub mod ecrecover;

use move_core_types::identifier::Identifier;
use move_core_types::language_storage::CORE_CODE_ADDRESS;
use move_vm_runtime::native_functions::{NativeFunction, NativeFunctionTable};
use u256::{
    native_u256_add, native_u256_div, native_u256_from_bytes, native_u256_mul, native_u256_pow,
    native_u256_rem, native_u256_sub,
};
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
#[repr(u8)]
pub enum NativeCostIndex {
    SHA2_256 = 0,
    SHA3_256 = 1,
    ED25519_VERIFY = 2,
    ED25519_THRESHOLD_VERIFY = 3,
    BCS_TO_BYTES = 4,
    LENGTH = 5,
    EMPTY = 6,
    BORROW = 7,
    BORROW_MUT = 8,
    PUSH_BACK = 9,
    POP_BACK = 10,
    DESTROY_EMPTY = 11,
    SWAP = 12,
    ED25519_VALIDATE_KEY = 13,
    SIGNER_BORROW = 14,
    CREATE_SIGNER = 15,
    DESTROY_SIGNER = 16,
    EMIT_EVENT = 17,
    BCS_TO_ADDRESS = 18,
    TOKEN_NAME_OF = 19,
    KECCAK_256 = 20,
    RIPEMD160 = 21,
    ECRECOVER = 22,
    U256_FROM_BYTES = 23,
    U256_ADD = 24,
    U256_SUB = 25,
    U256_MUL = 26,
    U256_DIV = 27,
    U256_REM = 28,
    U256_POW = 29,
}

impl NativeCostIndex {
    //note: should change this value when add new native function.
    pub const NUMBER_OF_NATIVE_FUNCTIONS: usize = 30;
}

/// The function returns all native functions supported by Starcoin.
/// NOTICE:
/// - mostly re-use natives defined in move-stdlib.
/// - be careful with the native cost table index used in the implementation
pub fn starcoin_natives() -> NativeFunctionTable {
    const NATIVES: &[(&str, &str, NativeFunction)] = &[
        (
            "Hash",
            "sha2_256",
            move_stdlib::natives::hash::native_sha2_256,
        ),
        (
            "Hash",
            "sha3_256",
            move_stdlib::natives::hash::native_sha3_256,
        ),
        ("Hash", "keccak_256", hash::native_keccak_256),
        ("Hash", "ripemd160", hash::native_ripemd160),
        (
            "BCS",
            "to_bytes",
            move_stdlib::natives::bcs::native_to_bytes,
        ),
        ("BCS", "to_address", bcs::native_to_address),
        (
            "Signature",
            "ed25519_validate_pubkey",
            signature::native_ed25519_publickey_validation,
        ),
        (
            "Signature",
            "ed25519_verify",
            signature::native_ed25519_signature_verification,
        ),
        ("Signature", "native_ecrecover", ecrecover::native_ecrecover),
        (
            "Vector",
            "length",
            move_stdlib::natives::vector::native_length,
        ),
        (
            "Vector",
            "empty",
            move_stdlib::natives::vector::native_empty,
        ),
        (
            "Vector",
            "borrow",
            move_stdlib::natives::vector::native_borrow,
        ),
        (
            "Vector",
            "borrow_mut",
            move_stdlib::natives::vector::native_borrow,
        ),
        (
            "Vector",
            "push_back",
            move_stdlib::natives::vector::native_push_back,
        ),
        (
            "Vector",
            "pop_back",
            move_stdlib::natives::vector::native_pop,
        ),
        (
            "Vector",
            "destroy_empty",
            move_stdlib::natives::vector::native_destroy_empty,
        ),
        ("Vector", "swap", move_stdlib::natives::vector::native_swap),
        (
            "Event",
            "write_to_event_store",
            move_stdlib::natives::event::native_write_to_event_store,
        ),
        ("Account", "create_signer", account::native_create_signer),
        ("Account", "destroy_signer", account::native_destroy_signer),
        (
            "Signer",
            "borrow_address",
            move_stdlib::natives::signer::native_borrow_address,
        ),
        ("Token", "name_of", token::native_token_name_of),
        ("Debug", "print", debug::native_print),
        (
            "Debug",
            "print_stack_trace",
            debug::native_print_stack_trace,
        ),
        #[cfg(feature = "testing")]
        (
            "UnitTest",
            "create_signers_for_testing",
            move_stdlib::natives::unit_test::native_create_signers_for_testing,
        ),
        ("U256", "from_bytes", native_u256_from_bytes),
        ("U256", "native_add", native_u256_add),
        ("U256", "native_sub", native_u256_sub),
        ("U256", "native_mul", native_u256_mul),
        ("U256", "native_div", native_u256_div),
        ("U256", "native_rem", native_u256_rem),
        ("U256", "native_pow", native_u256_pow),
    ];
    NATIVES
        .iter()
        .cloned()
        .map(|(module_name, func_name, func)| {
            (
                CORE_CODE_ADDRESS,
                Identifier::new(module_name).unwrap(),
                Identifier::new(func_name).unwrap(),
                func,
            )
        })
        .collect()
}
