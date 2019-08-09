use std::collections::HashMap;
use std::convert::TryInto;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    mpsc::{self, channel, Receiver, Sender},
    Arc, Mutex,
};
use std::thread;

use chrome_devtools_protocol::{
    api::Version, target, Browser, CallId, CallSite, Event, EventSource, Message, Method, Page,
    Response, Target,
};
use failure::{bail, Error};
use structopt::StructOpt;
use url::Url;
use websocket::{message::OwnedMessage, sender::Writer, stream::sync::TcpStream};

const DEFAULT_TARGET: &str = "http://localhost:9222";

#[derive(Debug, StructOpt)]
#[structopt(name = "hello", about = "Say hello to Chrome")]
struct Opt {
    /// Chrome DevTools endpoint (default: http://localhost:9222)
    #[structopt(name = "scheme://host[:port]")]
    target: Option<String>,
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
    pub fn new(uri: &Url) -> Result<Self, Error> {
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
                                Message::Event(Event::TargetReceivedMessageFromTarget(evt)) => {
                                    msg = Some(OwnedMessage::Text(evt.message));
                                }
                                Message::Event(evt) => event_queue.send(evt).unwrap(),
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

    fn call<T: Method>(&mut self, method: T) -> Result<T::ReturnObject, Self::Error> {
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

    fn events(&'a self) -> mpsc::Iter<'a, Event> {
        self.events.iter()
    }
}

struct Session {
    endpoint: Endpoint,
    session_id: String,
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

fn main() -> Result<(), Error> {
    let opt = Opt::from_args();

    let mut uri = Url::parse(&opt.target.unwrap_or(DEFAULT_TARGET.to_owned()))?;

    let ws_uri = match uri.scheme() {
        "ws" | "wss" => uri.clone(),
        "http" => {
            uri.set_path("/json/version");

            let version: Version = dbg!(reqwest::get(uri.as_str())?.json()?);

            Url::parse(&version.websocket_debugger_url)?
        }
        scheme @ _ => bail!("unsupport scheme: {}", scheme),
    };

    let mut browser = Endpoint::new(&ws_uri)?;
    let version = dbg!(browser.get_version()?);

    println!(
        "hello {} (V8 {}, devtools {})",
        version.product, version.js_version, version.protocol_version
    );

    browser.set_discover_targets(true)?;

    let mut target_id = None;

    for event in browser.events() {
        match dbg!(event) {
            Event::TargetCreated(target::TargetCreatedEvent { ref target_info })
                if target_info.r#type == "page" =>
            {
                target_id = Some(target_info.target_id.clone());

                break;
            }
            _ => {}
        }
    }

    let session_id = dbg!(browser.attach_to_target(target_id.unwrap(), None)?);
    let mut session = Session {
        endpoint: browser,
        session_id,
    };

    session.enable()?;
    session.set_lifecycle_events_enabled(true)?;

    let (frame_id, loader_id, err_msg) =
        dbg!(session.navigate("https://www.google.com/".to_owned(), None, None, None)?);

    Ok(())
}
