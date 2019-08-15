use std::collections::HashMap;
use std::fs;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc, Mutex,
};

use failure::{bail, err_msg, format_err, Error, Fail, Fallible, ResultExt};
use futures::{
    future::{self, Either},
    prelude::*,
    stream::{SplitSink, SplitStream},
    sync::{mpsc, oneshot, BiLock, BiLockAcquired},
    try_ready,
};
use structopt::StructOpt;
use url::Url;
use websocket::{
    client::r#async::Client, message::OwnedMessage, r#async::TcpStream, ClientBuilder,
};

use chrome_devtools_protocol::{
    api::Version, network, page, target, AsyncCallSite, AsyncEventSource, AsyncPage, AsyncTarget,
    CallId, Event, Message, Method, Response,
};

#[derive(Debug, StructOpt)]
#[structopt(name = "take_screenshot", about = "Take screenshot for the web page")]
struct Opt {
    /// Chrome DevTools endpoint (default: http://localhost:9222)
    #[structopt(
        name = "scheme://host[:port]",
        long = "endpoint",
        short = "e",
        default_value = "http://localhost:9222"
    )]
    endpoint: String,

    #[structopt(default_value = "https://www.wikipedia.org")]
    url: Url,
}

impl Opt {
    fn websocket_debugger_url(&self) -> Fallible<Url> {
        let mut uri = Url::parse(self.endpoint.as_str())?;

        match uri.scheme() {
            "ws" | "wss" => Ok(uri),
            "http" => {
                uri.set_path("/json/version");

                let version: Version = dbg!(reqwest::get(uri.as_str())?.json()?);

                Ok(Url::parse(&version.websocket_debugger_url)?)
            }
            scheme @ _ => bail!("unsupport scheme: {}", scheme),
        }
    }
}

type Requests = Arc<Mutex<HashMap<CallId, oneshot::Sender<Response>>>>;

struct Endpoint {
    writer: SplitSink<Client<TcpStream>>,
    events: mpsc::UnboundedReceiver<Event>,
    requests: Requests,
    call_id: AtomicUsize,
    running: BiLockAcquired<()>,
}

unsafe impl Send for Endpoint {}

impl AsyncCallSite for Endpoint {
    type Error = Error;

    fn async_call<M>(
        self,
        method: M,
    ) -> Box<dyn futures::Future<Item = (M::ReturnObject, Self), Error = Self::Error> + Send>
    where
        M: Method,
        M::ReturnObject: 'static + Send,
    {
        let call = dbg!(Method::to_method_call(method, self.next_call_id()));
        let message = dbg!(serde_json::to_string(&call).unwrap());

        let (sender, receiver) = oneshot::channel();

        let Endpoint {
            writer,
            events,
            requests,
            call_id,
            running,
        } = self;

        requests.lock().unwrap().insert(call.id(), sender);

        Box::new(
            writer
                .send(OwnedMessage::Text(message))
                .and_then(|writer| writer.flush())
                .map_err(|err| err.context("send call request").into())
                .and_then(move |writer| {
                    receiver
                        .map_err(|err| err.context("recv call reponse").into())
                        .and_then(|res| dbg!(res).into_result())
                        .map(|res| {
                            (
                                res,
                                Endpoint {
                                    writer,
                                    events,
                                    requests,
                                    call_id,
                                    running,
                                },
                            )
                        })
                }),
        )
    }
}

impl AsyncEventSource for Endpoint {
    type Error = ();
    type Event = Event;
    type Events = mpsc::UnboundedReceiver<Event>;

    fn events(self) -> Self::Events {
        self.events
    }
}

impl Endpoint {
    fn new(client: Client<TcpStream>) -> Self {
        let (writer, reader) = client.split();
        let (sender, receiver) = mpsc::unbounded();
        let requests = Arc::new(Mutex::new(HashMap::new()));
        let dispatcher = Dispatcher {
            reader,
            requests: requests.clone(),
            events: sender,
        };
        let (terminated, running) = BiLock::new(());
        let running = running.lock().wait().unwrap();

        tokio::spawn(
            terminated
                .lock()
                .map_err(|_| eprintln!("lock failed"))
                .select2(
                    dispatcher
                        .for_each(|_| Ok(()))
                        .map_err(|err| eprintln!("dispach message: {}", err)),
                )
                .map(|_| ())
                .map_err(|_| ()),
        );

        Endpoint {
            writer,
            events: receiver,
            requests,
            call_id: AtomicUsize::new(0),
            running,
        }
    }

    fn next_call_id(&self) -> CallId {
        self.call_id.fetch_add(1, Ordering::Relaxed)
    }

    fn wait_until_event<F, B>(self, f: F) -> impl Future<Item = (Option<B>, Self), Error = Error>
    where
        F: FnMut(Event) -> Option<B>,
    {
        let Endpoint {
            writer,
            events,
            requests,
            call_id,
            running,
        } = self;

        events
            .filter_map(f)
            .into_future()
            .map(|(res, events)| {
                (
                    res,
                    Endpoint {
                        writer,
                        events: events.into_inner(),
                        requests,
                        call_id,
                        running,
                    },
                )
            })
            .map_err(|_| err_msg("wait event"))
    }

    fn wait_for_initial_tab(
        self,
    ) -> impl Future<Item = (Option<target::TargetInfo>, Self), Error = Error> {
        self.wait_until_event(|evt| {
            if let Event::TargetCreated(evt) = evt {
                if evt.target_info.r#type == "page" {
                    Some(evt.target_info)
                } else {
                    None
                }
            } else {
                None
            }
        })
    }
}

struct Dispatcher {
    reader: SplitStream<Client<TcpStream>>,
    requests: Requests,
    events: mpsc::UnboundedSender<Event>,
}

impl Stream for Dispatcher {
    type Item = ();
    type Error = Error;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        let mut msg = try_ready!(self
            .reader
            .poll()
            .map_err(|err| format_err!("recv call response, {}", err)));

        if msg.is_none() {
            return Ok(Async::Ready(None));
        }

        while msg.is_some() {
            match msg.take().unwrap() {
                OwnedMessage::Text(text) => {
                    let m = text
                        .parse()
                        .map_err(|err| format_err!("parse message, {}", err))?;

                    match dbg!(m) {
                        Message::Event(evt) => {
                            if let Event::TargetReceivedMessageFromTarget(evt) = evt {
                                msg = Some(OwnedMessage::Text(evt.message));
                            } else {
                                self.events.unbounded_send(evt).context("send event")?;
                            }
                        }
                        Message::Response(res) => {
                            if let Some(sender) = self.requests.lock().unwrap().remove(&res.call_id)
                            {
                                sender
                                    .send(res)
                                    .map_err(|res| format_err!("send response: {:?}", res))?;
                            }
                        }
                        Message::ConnectionShutdown => {
                            return Ok(Async::Ready(None));
                        }
                    }
                }
                OwnedMessage::Close(_) => {
                    return Ok(Async::Ready(None));
                }
                msg => {
                    dbg!(format!("skip {:?}", msg));
                }
            }
        }

        Ok(Async::Ready(Some(())))
    }
}

struct Session {
    endpoint: Endpoint,
    session_id: String,
}

impl AsyncCallSite for Session {
    type Error = Error;

    fn async_call<M>(
        self,
        method: M,
    ) -> Box<dyn futures::Future<Item = (M::ReturnObject, Self), Error = Self::Error> + Send>
    where
        M: Method,
        M::ReturnObject: 'static + Send,
    {
        let call = dbg!(Method::to_method_call(method, self.endpoint.next_call_id()));
        let message = dbg!(serde_json::to_string(&call).unwrap());

        let (sender, receiver) = oneshot::channel();

        let Session {
            endpoint,
            session_id,
        } = self;

        endpoint.requests.lock().unwrap().insert(call.id(), sender);

        Box::new(
            endpoint
                .send_message_to_target(message, Some(session_id.clone()), None)
                .and_then(|(_, endpoint)| {
                    receiver
                        .map_err(|err| err.context("recv response").into())
                        .and_then(|res| dbg!(res).into_result())
                        .map(|res| {
                            (
                                res,
                                Session {
                                    endpoint,
                                    session_id,
                                },
                            )
                        })
                }),
        )
    }
}

impl Session {
    fn wait_until_event<F, B>(self, f: F) -> impl Future<Item = (Option<B>, Self), Error = Error>
    where
        F: FnMut(Event) -> Option<B>,
    {
        let Session {
            endpoint,
            session_id,
        } = self;

        endpoint.wait_until_event(f).map(|(res, endpoint)| {
            (
                res,
                Session {
                    endpoint,
                    session_id,
                },
            )
        })
    }

    fn wait_until_navigated(
        self,
        frame_id: page::FrameId,
        loader_id: Option<network::LoaderId>,
    ) -> impl Future<Item = (Option<page::LifecycleEvent>, Self), Error = Error> {
        self.wait_until_event(move |evt| {
            if let Event::PageLifecycleEvent(evt) = evt {
                if evt.frame_id == frame_id
                    && loader_id
                        .as_ref()
                        .map_or(true, |loader_id| evt.loader_id == *loader_id)
                    && (evt.name == "networkAlmostIdle" || evt.name == "init")
                {
                    Some(evt)
                } else {
                    None
                }
            } else {
                None
            }
        })
    }
}

fn main() -> Fallible<()> {
    pretty_env_logger::init();

    let opt = Opt::from_args();

    let ws_uri = opt.websocket_debugger_url()?;
    let target_url = opt.url.to_string();

    let client = ClientBuilder::new(ws_uri.as_str())?
        .async_connect_insecure()
        .map_err(move |err| err.context(format!("connect {}", ws_uri)).into())
        .map(move |(client, _headers)| Endpoint::new(client))
        .and_then(|endpoint| endpoint.set_discover_targets(true))
        .and_then(|(_, endpoint)| endpoint.wait_for_initial_tab())
        .and_then(|(target_info, endpoint)| {
            endpoint.attach_to_target(target_info.unwrap().target_id, None)
        })
        .map(|(session_id, endpoint)| Session {
            endpoint,
            session_id,
        })
        .and_then(|session| session.enable())
        .and_then(|(_, session)| session.set_lifecycle_events_enabled(true))
        .and_then(|(_, session)| session.navigate(target_url, None, None, None))
        .and_then(|((frame_id, loader_id, err_msg), session)| {
            if let Some(msg) = err_msg {
                Either::A(future::err(format_err!("fail to navigate, {}", msg)))
            } else {
                Either::B(session.wait_until_navigated(frame_id, loader_id))
            }
        })
        .and_then(|(_, session)| {
            session.capture_screenshot(
                Some(page::CaptureScreenshotRequestFormat::Png),
                None,
                None,
                None,
            )
        })
        .and_then(|(data, _)| {
            fs::write("output.png", &data).map_err(|err| err.context("write image").into())
        })
        .map(|_| ())
        .map_err(|err| eprintln!("error: {:#?}", err));

    tokio::run(client);

    Ok(())
}
