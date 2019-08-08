use std::collections::HashMap;
use std::convert::TryInto;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    mpsc::{channel, Receiver, Sender},
    Arc, Mutex,
};
use std::thread;

use chrome_devtools_protocol::{
    Browser, CallId, CallSite, Event, Message, Method, Response, Version,
};
use failure::{bail, Error};
use structopt::StructOpt;
use url::Url;
use websocket::{
    client::sync::Client, message::OwnedMessage, sender::Writer, stream::sync::TcpStream,
};

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
    requests: Arc<Mutex<HashMap<CallId, Sender<Response>>>>,
    events: EventQueue,
}

impl Endpoint {
    pub fn new(client: Client<TcpStream>) -> Result<Self, Error> {
        let (mut receiver, sender) = client.split()?;

        let (event_queue, events) = channel();
        let requests = Arc::new(Mutex::new(HashMap::new()));
        {
            let requests = Arc::clone(&requests);

            thread::spawn(move || {
                for msg in receiver.incoming_messages() {
                    dispatch_message(&requests, &event_queue, msg.unwrap()).unwrap()
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

    pub fn events(&self) -> &EventQueue {
        &self.events
    }
}

fn dispatch_message(
    requests: &RequestQueue,
    events: &Sender<Event>,
    msg: OwnedMessage,
) -> Result<(), Error> {
    match msg {
        OwnedMessage::Text(text) => match text.parse()? {
            Message::Event(evt) => events.send(evt)?,
            Message::Response(res) => {
                if let Some(sender) = requests.lock().unwrap().remove(&res.call_id) {
                    sender.send(res)?;
                }
            }
            Message::ConnectionShutdown => bail!("connection shutdown"),
        },
        OwnedMessage::Close(close) => bail!("ws channel closed, {:?}", close),
        _msg => {
            // ignore message
        }
    }

    Ok(())
}

impl CallSite for Endpoint {
    type Error = Error;

    fn call<T: Method>(&mut self, method: T) -> Result<T::ReturnObject, Self::Error> {
        let call = method.to_method_call(self.call_id.fetch_add(1, Ordering::Relaxed));
        let json = serde_json::to_string(&call)?;
        let msg = websocket::Message::text(json);

        let (sender, receiver) = channel();

        self.requests.lock().unwrap().insert(call.id(), sender);
        self.sender.send_message(&msg)?;

        receiver.recv()?.into_result()
    }
}

fn main() -> Result<(), Error> {
    let opt = Opt::from_args();

    let mut uri = Url::parse(&opt.target.unwrap_or(DEFAULT_TARGET.to_owned()))?;

    let ws_uri = match uri.scheme() {
        "ws" | "wss" => uri,
        "http" => {
            uri.set_path("/json/version");

            let version: Version = reqwest::get(uri.as_str())?.json()?;

            Url::parse(&version.websocket_debugger_url)?
        }
        scheme @ _ => bail!("unsupport scheme: {}", scheme),
    };

    let client = websocket::ClientBuilder::new(ws_uri.as_str())?.connect_insecure()?;
    let mut endpoint = Endpoint::new(client)?;

    let version = endpoint.get_version()?;

    println!(
        "hello {} (V8 {}, devtools {})",
        version.product, version.js_version, version.protocol_version
    );

    Ok(())
}
