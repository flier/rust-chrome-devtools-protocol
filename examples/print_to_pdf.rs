use std::collections::HashMap;
use std::fs;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    mpsc::{self, channel, Receiver, Sender},
    Arc, Mutex,
};
use std::thread;

use chrome_devtools_protocol::{
    api::Version, network, page, target, Browser, CallId, CallSite, Event, EventSource, Message,
    Method, Page, Response, Target,
};
use failure::{bail, Error, Fallible};
use structopt::StructOpt;
use url::Url;
use websocket::{message::OwnedMessage, sender::Writer, stream::sync::TcpStream};

#[derive(Debug, StructOpt)]
#[structopt(name = "print_to_pdf", about = "Print the web page to a PDF file")]
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

type RequestQueue = Arc<Mutex<HashMap<CallId, Sender<Response>>>>;
type EventQueue = Receiver<Event>;

struct Endpoint {
    call_id: AtomicUsize,
    sender: Writer<TcpStream>,
    requests: RequestQueue,
    events: EventQueue,
}

impl Endpoint {
    pub fn new(uri: &Url) -> Fallible<Self> {
        let client = websocket::ClientBuilder::new(uri.as_str())?.connect_insecure()?;
        let (mut receiver, sender) = client.split()?;
        let (event_queue, events) = channel();
        let requests = Arc::new(Mutex::new(HashMap::new()));
        {
            let request_queue: RequestQueue = Arc::clone(&requests);

            thread::spawn(move || {
                for msg in receiver.incoming_messages() {
                    let mut msg = msg.ok();

                    while msg.is_some() {
                        match msg.take().unwrap() {
                            OwnedMessage::Text(text) => match dbg!(text.parse().unwrap()) {
                                Message::Event(evt) => {
                                    if let Event::TargetReceivedMessageFromTarget(evt) = evt {
                                        msg = Some(OwnedMessage::Text(evt.message));
                                    } else {
                                        event_queue.send(evt).unwrap()
                                    }
                                }
                                Message::Response(res) => {
                                    if let Some(sender) =
                                        request_queue.lock().unwrap().remove(&res.call_id)
                                    {
                                        sender.send(res).unwrap();
                                    }
                                }
                                Message::ConnectionShutdown => return,
                            },
                            OwnedMessage::Close(_) => return,
                            _msg => {
                                // ignore message
                            }
                        }
                    }
                }
            });
        }

        Ok(Endpoint {
            call_id: AtomicUsize::new(0),
            sender,
            requests,
            events,
        })
    }

    fn next_call_id(&self) -> CallId {
        self.call_id.fetch_add(1, Ordering::Relaxed)
    }
}

impl CallSite for Endpoint {
    type Error = Error;

    fn call<M>(&mut self, method: M) -> Result<M::ReturnObject, Self::Error>
    where
        M: Method,
        M::ReturnObject: 'static,
    {
        let call = dbg!(Method::to_method_call(method, self.next_call_id()));
        let json = dbg!(serde_json::to_string(&call)?);
        let msg = websocket::Message::text(json);

        let (sender, receiver) = channel();
        self.requests.lock().unwrap().insert(call.id(), sender);
        self.sender.send_message(&msg)?;

        receiver.recv()?.into_result()
    }
}

impl<'a> EventSource<'a> for Endpoint {
    type Event = Event;
    type Events = mpsc::Iter<'a, Event>;

    fn events(&'a mut self) -> mpsc::Iter<'a, Event> {
        self.events.iter()
    }
}

impl Endpoint {
    fn wait_for_initial_tab(&mut self) -> Option<target::TargetInfo> {
        self.events().find_map(|evt| {
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

struct Session {
    endpoint: Endpoint,
    session_id: String,
}

impl Session {
    fn browser(&mut self) -> &mut Browser<Error = Error> {
        &mut self.endpoint
    }

    fn wait_until_navigated(
        &mut self,
        frame_id: page::FrameId,
        loader_id: Option<network::LoaderId>,
    ) -> Option<page::LifecycleEvent> {
        self.endpoint.events().find_map(|evt| {
            if let Event::PageLifecycleEvent(evt) = evt {
                if evt.frame_id == frame_id
                    && loader_id
                        .as_ref()
                        .map_or(true, |loader_id| loader_id == &evt.loader_id)
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

impl CallSite for Session {
    type Error = Error;

    fn call<T: Method>(&mut self, method: T) -> Result<T::ReturnObject, Self::Error> {
        let call = dbg!(Method::to_method_call(method, self.endpoint.next_call_id()));
        let message = serde_json::to_string(&call)?;

        let (sender, receiver) = channel();
        self.endpoint
            .requests
            .lock()
            .unwrap()
            .insert(call.id(), sender);

        self.endpoint
            .send_message_to_target(message, Some(self.session_id.clone()), None)?;

        receiver.recv()?.into_result()
    }
}

fn main() -> Fallible<()> {
    pretty_env_logger::init();

    let opt = Opt::from_args();

    let ws_uri = opt.websocket_debugger_url()?;
    let mut browser = Endpoint::new(&ws_uri)?;
    let _version = dbg!(browser.get_version()?);

    browser.set_discover_targets(true)?;

    let target_id = browser.wait_for_initial_tab().unwrap().target_id;
    let session_id = dbg!(browser.attach_to_target(target_id, None)?);

    let mut session = Session {
        endpoint: browser,
        session_id,
    };

    session.enable()?;
    session.set_lifecycle_events_enabled(true)?;

    let (frame_id, loader_id, err_msg) =
        dbg!(session.navigate(opt.url.to_string(), None, None, None))?;

    if let Some(msg) = err_msg {
        bail!("navigate failed, {}", msg);
    }

    dbg!(session.wait_until_navigated(frame_id, loader_id)).unwrap();

    let (data, _stream) = session.print_to_pdf(Default::default())?;

    fs::write("output.pdf", data)?;

    Ok(())
}
