// Copyright (c) The Starcoin Core Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::Result;
use cucumber::{after, before, cucumber, Steps, StepsBuilder};
use scmd::CmdContext;
use starcoin_cmd::*;
use starcoin_config::{NodeConfig, StarcoinOpt};
use starcoin_logger::prelude::*;
use starcoin_node::NodeHandle;
use starcoin_rpc_client::RpcClient;
use starcoin_storage::cache_storage::CacheStorage;
use starcoin_storage::db_storage::DBStorage;
use starcoin_storage::storage::StorageInstance;
use starcoin_storage::Storage;
use starcoin_types::account_address::AccountAddress;
use starcoin_wallet_api::WalletAccount;
use std::borrow::Borrow;
use std::sync::Arc;
use std::time::Duration;
use steps::{cmd as steps_cmd, node as steps_node, state as steps_state, sync, transaction};

mod steps;

#[derive(Default)]
pub struct MyWorld {
    node_config: Option<NodeConfig>,
    storage: Option<Storage>,
    rpc_client: Option<RpcClient>,
    local_rpc_client: Option<RpcClient>,
    default_account: Option<WalletAccount>,
    txn_account: Option<WalletAccount>,
    node_handle: Option<NodeHandle>,
    context: Option<CmdContext<CliState, StarcoinOpt>>,
    default_address: Option<AccountAddress>,
}
impl MyWorld {
    pub fn storage(&self) -> Option<&Storage> {
        match &self.storage {
            Some(storage) => Some(storage),
            _ => None,
        }
    }
}

impl cucumber::World for MyWorld {}

pub fn steps() -> Steps<MyWorld> {
    let mut builder: StepsBuilder<MyWorld> = Default::default();
    builder
        .given("a storage", |world: &mut MyWorld, _step| {
            let cache_storage = Arc::new(CacheStorage::new());
            let db_storage = Arc::new(DBStorage::new(starcoin_config::temp_path().as_ref()));
            let storage = Storage::new(StorageInstance::new_cache_and_db_instance(
                cache_storage,
                db_storage,
            ))
            .unwrap();
            info!("storage created!");
            world.storage = Some(storage)
        })
        .given("remote rpc client", |world: &mut MyWorld, _step| {
            let client = RpcClient::connect_websocket(env!("STARCOIN_WS")).unwrap();
            info!("rpc client created!");
            world.rpc_client = Some(client)
        })
        .given("dev rpc client", |world: &mut MyWorld, _step| {
            let node_config = world.node_config.as_ref().take().unwrap();
            let client = RpcClient::connect_ipc(node_config.clone().rpc.get_ipc_file()).unwrap();
            info!("dev node local rpc client created!");
            world.rpc_client = Some(client)
        })
        .given("default account", |world: &mut MyWorld, _step| {
            let client = world.rpc_client.as_ref().take().unwrap();
            let default_account = client.clone().wallet_default().unwrap().unwrap();
            info!("default account config success!");
            client
                .wallet_unlock(
                    default_account.address,
                    "".parse().unwrap(),
                    Duration::from_secs(300 as u64),
                )
                .unwrap();
            world.default_account = Some(default_account)
        })
        .given("an account", |world: &mut MyWorld, _step| {
            let client = world.rpc_client.as_ref().take().unwrap();
            let password = "integration";
            let account = client
                .clone()
                .wallet_create(password.clone().parse().unwrap())
                .unwrap();
            client
                .wallet_unlock(
                    account.address,
                    password.clone().parse().unwrap(),
                    Duration::from_secs(300 as u64),
                )
                .unwrap();
            info!("a account create success!");
            world.txn_account = Some(account.clone())
        })
        .given("cmd context", |world: &mut MyWorld, _step| {
            let context = CmdContext::<CliState, StarcoinOpt>::with_default_action(
                |_global_opt| -> Result<CliState> {
                    let client = RpcClient::connect_websocket(env!("STARCOIN_WS")).unwrap();
                    let node_info = client.borrow().node_info()?;
                    let state = CliState::new(node_info.net, client, None);
                    Ok(state)
                },
                |_, _, state| {
                    let (_, _, handle) = state.into_inner();
                    if let Some(handle) = handle {
                        if let Err(e) = handle.join() {
                            error!("{:?}", e);
                        }
                    }
                },
                |_app, _opt, _state| {},
                |_app, _opt, _state| {},
            );
            world.context = Some(context);
        });
    builder.build()
}

// Declares a before handler function named `a_before_fn`
before!(a_before_fn => |_scenario| {
});

// Declares an after handler function named `an_after_fn`
after!(an_after_fn => |_scenario| {
});

// A setup function to be called before everything else
fn setup() {}

cucumber! {
    features: "./features", // Path to our feature files
    world: MyWorld, // The world needs to be the same for steps and the main cucumber call
    steps: &[
        crate::steps, // the `steps!` macro creates a `steps` function in a module
        transaction::steps,
        steps_node::steps,
        sync::steps,
        steps_state::steps,
        steps_cmd::steps,
    ],
    setup: setup, // Optional; called once before everything
    before: &[
        a_before_fn // Optional; called before each scenario
    ],
    after: &[
        an_after_fn // Optional; called after each scenario
    ]
}
