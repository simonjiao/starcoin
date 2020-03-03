/// Sync message which outbound
use crate::pool::TTLPool;
use crate::{do_duration, DELAY_TIME};
use actix::prelude::*;
use actix::{
    fut::wrap_future, fut::FutureWrap, Actor, Addr, AsyncContext, Context, Handler,
    ResponseActFuture,
};
use anyhow::{Error, Result};
use atomic_refcell::AtomicRefCell;
use bus::{Bus, BusActor, Subscription};
use chain::{ChainActor, ChainActorRef};
use crypto::hash::CryptoHash;
use futures::compat::Future01CompatExt;
use futures_locks::{Mutex, RwLock};
use futures_timer::Delay;
use itertools;
use network::sync_messages::{
    BatchBodyMsg, BatchHashByNumberMsg, BatchHeaderMsg, BlockBody, DataType, DownloadMessage,
    GetDataByHashMsg, GetHashByNumberMsg, HashWithNumber, LatestStateMsg,
    ProcessMessage,
};
use network::{NetworkAsyncService, RPCMessage, RPCRequest, RPCResponse};
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use traits::{AsyncChain, ChainAsyncService};
use txpool::TxPoolRef;
use types::{
    block::{Block, BlockHeader},
    peer_info::PeerInfo,
};
use futures::channel::mpsc;

#[derive(Default, Debug, Message)]
#[rtype(result = "Result<()>")]
struct SyncEvent {}

#[derive(Clone)]
pub struct DownloadActor {
    downloader: Arc<Downloader>,
    peer_info: Arc<PeerInfo>,
    network: NetworkAsyncService<TxPoolRef>,
    bus: Addr<BusActor>,
    sync_event_sender: mpsc::Sender<SyncEvent>,
    sync_duration: Duration,
}

impl DownloadActor {
    pub fn launch(
        peer_info: Arc<PeerInfo>,
        chain_reader: ChainActorRef<ChainActor>,
        network: NetworkAsyncService<TxPoolRef>,
        bus: Addr<BusActor>,
    ) -> Result<Addr<DownloadActor>> {
        let download_actor = DownloadActor::create(move |ctx| {
            let (sync_event_sender, sync_event_receiver) = mpsc::channel(100);
            ctx.add_message_stream(sync_event_receiver);
            DownloadActor {
                downloader: Arc::new(Downloader::new(chain_reader)),
                peer_info,
                network,
                bus,
                sync_event_sender,
                sync_duration: Duration::from_secs(5),
            }
        });
        Ok(download_actor)
    }
}

impl Actor for DownloadActor {
    type Context = Context<Self>;

    fn started(&mut self, ctx: &mut Self::Context) {
        ctx.run_interval(self.sync_duration, move |act, _ctx| {
            act.sync_event_sender.try_send(SyncEvent {});
        });
        info!("download actor started.")
    }
}

impl Handler<SyncEvent> for DownloadActor {
    type Result = Result<()>;
    fn handle(&mut self, _item: SyncEvent, _ctx: &mut Self::Context) -> Self::Result {
        Self::sync_from_best_peer(self.downloader.clone(), self.network.clone());
        Ok(())
    }
}

impl Handler<DownloadMessage> for DownloadActor {
    type Result = ResponseActFuture<Self, Result<()>>;

    fn handle(&mut self, msg: DownloadMessage, ctx: &mut Self::Context) -> Self::Result {
        let downloader = self.downloader.clone();
        let my_peer_info = self.peer_info.id.clone();
        let network = self.network.clone();
        let fut = async move {
            match msg {
                DownloadMessage::LatestStateMsg(peer_info, latest_state_msg) => {
                    debug!(
                        "latest_state_msg number: {:?}",
                        &latest_state_msg.header.number()
                    );
                    Downloader::handle_latest_state_msg(
                        downloader.clone(),
                        peer_info.clone(),
                        latest_state_msg,
                    )
                        .await;
                }
                DownloadMessage::NewHeadBlock(peer_info, block) => {
                    info!("receive new block: {:?} from {:?}", block.header().id(), peer_info.id);
                    //1. update latest block
                    let latest_state_msg = LatestStateMsg { header: block.header().clone() };
                    Downloader::handle_latest_state_msg(
                        downloader.clone(),
                        peer_info.clone(),
                        latest_state_msg,
                    )
                        .await;

                    //2. connect block
                    Downloader::do_block(downloader.clone(), block).await;
                }
                DownloadMessage::MinedBlock(block) => {
                    info!("new block: {:?}", block.header().id());
                    Downloader::do_block(downloader.clone(), block).await;
                }
                _ => {}
            }

            Ok(())
        };

        Box::new(wrap_future::<_, Self>(fut))
    }
}

impl DownloadActor {
    fn sync_from_best_peer(downloader: Arc<Downloader>, network: NetworkAsyncService<TxPoolRef>) {
        Arbiter::spawn(async move {
            println!("begin sync.");
            if let Some(best_peer) = Downloader::best_peer(downloader.clone()).await {
                let send_get_hash_by_number_msg =
                    Downloader::send_get_hash_by_number_msg(downloader.clone(), best_peer.clone()).await;
                match send_get_hash_by_number_msg {
                    Some(get_hash_by_number_msg) => {
                        info!(
                            "best peer: {:?} , numbers : {}",
                            best_peer.clone(),
                            get_hash_by_number_msg.numbers.len()
                        );
                        let get_hash_by_number_req = RPCRequest::GetHashByNumberMsg(
                            ProcessMessage::GetHashByNumberMsg(get_hash_by_number_msg),
                        );

                        if let RPCResponse::BatchHashByNumberMsg(batch_hash_by_number_msg) =
                        network
                            .clone()
                            .send_request(
                                best_peer.id.clone(),
                                get_hash_by_number_req.clone(),
                                do_duration(DELAY_TIME),
                            )
                            .await
                            .unwrap()
                        {
                            debug!("batch_hash_by_number_msg:{:?}", batch_hash_by_number_msg);
                            let hash_with_number = Downloader::find_ancestor(
                                downloader.clone(),
                                best_peer.clone(),
                                batch_hash_by_number_msg,
                            )
                                .await;
                            debug!("hash_with_number:{:?}", hash_with_number);
                            match hash_with_number {
                                Some(_) => {
                                    let send_get_header_by_hash_msg =
                                        Downloader::send_get_header_by_hash_msg(
                                            downloader.clone(),
                                        )
                                            .await;
                                    match send_get_header_by_hash_msg {
                                        Some(get_data_by_hash_msg) => {
                                            let get_data_by_hash_req =
                                                RPCRequest::GetDataByHashMsg(
                                                    ProcessMessage::GetDataByHashMsg(
                                                        get_data_by_hash_msg,
                                                    ),
                                                );

                                            if let RPCResponse::BatchHeaderAndBodyMsg(
                                                _,
                                                headers,
                                                bodies,
                                            ) = network
                                                .clone()
                                                .send_request(
                                                    best_peer.id.clone(),
                                                    get_data_by_hash_req.clone(),
                                                    do_duration(DELAY_TIME),
                                                )
                                                .await
                                                .unwrap()
                                            {
                                                Downloader::do_blocks(
                                                    downloader.clone(),
                                                    headers.headers,
                                                    bodies.bodies,
                                                )
                                                    .await;
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
            }
            println!("end sync.");
        });
    }
}

/// Send download message
pub struct Downloader {
    hash_pool: TTLPool<HashWithNumber>,
    header_pool: TTLPool<BlockHeader>,
    body_pool: TTLPool<BlockBody>,
    peers: Arc<RwLock<HashMap<PeerInfo, LatestStateMsg>>>,
    chain_reader: ChainActorRef<ChainActor>,
}

const HEAD_CT: u64 = 100;

impl Downloader {
    pub fn new(chain_reader: ChainActorRef<ChainActor>) -> Self {
        Downloader {
            hash_pool: TTLPool::new(),
            header_pool: TTLPool::new(),
            body_pool: TTLPool::new(),
            //            _network: network,
            peers: Arc::new(RwLock::new(HashMap::new())),
            chain_reader,
        }
    }

    pub async fn handle_latest_state_msg(
        downloader: Arc<Downloader>,
        peer: PeerInfo,
        latest_state_msg: LatestStateMsg,
    ) {
        // let hash_num = HashWithNumber {
        //     hash: latest_state_msg.hash_header.hash.clone(),
        //     number: latest_state_msg.hash_header.header.number(),
        // };
        //        self.hash_pool
        //            .insert(peer.clone(), latest_state_msg.header.number(), hash_num);
        let mut lock = downloader
            .peers
            .write()
            .compat()
            .await
            .unwrap();
        if lock.get(&peer).is_none() || lock.get(&peer).unwrap().header.number() < latest_state_msg.header.number() {
            lock.insert(peer, latest_state_msg.clone());
        }
    }

    async fn best_peer(downloader: Arc<Downloader>) -> Option<PeerInfo> {
        let lock = downloader
            .peers
            .read()
            .compat()
            .await
            .unwrap();
        println!("size : {}", lock.len());
        for p in lock.keys()
        {
            return Some(p.clone());
        }

        println!("best peer return none.");
        None
    }

    pub async fn send_get_hash_by_number_msg(
        downloader: Arc<Downloader>,
        peer: PeerInfo,
    ) -> Option<GetHashByNumberMsg> {
        //todo：binary search

        let latest_number = downloader
            .chain_reader
            .clone()
            .current_header()
            .await
            .unwrap()
            .number();
        let number = downloader
            .peers
            .read()
            .compat()
            .await
            .unwrap()
            .get(&peer)
            .expect("Latest state is none.")
            .header
            .number();
        if latest_number < number {
            let mut numbers = Vec::new();
            if number < HEAD_CT {
                for i in 0..(number + 1) {
                    numbers.push(i);
                }
            } else {
                for i in 0..HEAD_CT {
                    numbers.push((number - HEAD_CT + i + 1));
                }
            };

            Some(GetHashByNumberMsg { numbers })
        } else {
            None
        }
    }

    pub async fn find_ancestor(
        downloader: Arc<Downloader>,
        peer: PeerInfo,
        batch_hash_by_number_msg: BatchHashByNumberMsg,
    ) -> Option<HashWithNumber> {
        //TODO
        let mut exist_ancestor = false;
        let mut ancestor = None;
        let mut hashs = batch_hash_by_number_msg.hashs.clone();
        let mut not_exist_hash = Vec::new();
        hashs.reverse();
        let id = downloader
            .chain_reader
            .clone()
            .current_header()
            .await
            .unwrap()
            .id();
        for hash in hashs {
            if downloader
                .chain_reader
                .clone()
                .get_block_by_hash(&hash.hash)
                .await
                .is_some()
            {
                exist_ancestor = true;
                info!("find ancestor hash : {:?}", hash);
                ancestor = Some(hash);
                break;
            } else {
                not_exist_hash.push(hash);
            }
        }

        if exist_ancestor {
            for hash in not_exist_hash {
                downloader
                    .hash_pool
                    .insert(peer.clone(), hash.number.clone(), hash);
            }
        }
        ancestor
    }

    pub async fn send_get_header_by_hash_msg(
        downloader: Arc<Downloader>,
    ) -> Option<GetDataByHashMsg> {
        let hash_vec = downloader.hash_pool.take(100);
        if !hash_vec.is_empty() {
            let mut hashs = hash_vec.iter().map(|hash| hash.hash).collect();
            Some(GetDataByHashMsg {
                hashs,
                data_type: DataType::HEADER,
            })
        } else {
            None
        }
    }

    pub async fn handle_batch_header_msg(
        downloader: Arc<Downloader>,
        peer: PeerInfo,
        batch_header_msg: BatchHeaderMsg,
    ) {
        if !batch_header_msg.headers.is_empty() {
            for header in batch_header_msg.headers {
                downloader
                    .header_pool
                    .insert(peer.clone(), header.number(), header);
            }
        }
    }

    pub async fn send_get_body_by_hash_msg(
        downloader: Arc<Downloader>,
    ) -> Option<GetDataByHashMsg> {
        let header_vec = downloader.header_pool.take(100);
        if !header_vec.is_empty() {
            let mut hashs = header_vec.iter().map(|header| header.id()).collect();
            Some(GetDataByHashMsg {
                hashs,
                data_type: DataType::BODY,
            })
        } else {
            None
        }
    }

    pub async fn do_blocks(
        downloader: Arc<Downloader>,
        headers: Vec<BlockHeader>,
        bodies: Vec<BlockBody>,
    ) {
        for (header, body) in itertools::zip_eq(headers, bodies) {
            let block = Block::new(header, body.transactions);
            //todo:verify block
            let _ = Self::do_block(downloader.clone(), block).await;
        }
    }

    pub async fn do_block(downloader: Arc<Downloader>, block: Block) {
        info!("do block {:?}", block.header().id());
        //todo:verify block
        let _ = downloader.chain_reader.clone().try_connect(block).await;
    }
}
