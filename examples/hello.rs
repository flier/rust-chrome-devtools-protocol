use std::collections::HashMap;
use std::convert::TryInto;
use std::sync::{
    mpsc::{channel, Sender},
    Arc, Mutex,
};
use std::thread;

use chrome_devtools_protocol::{CallId, CallSite, Message, Method};
use failure::{bail, Error};
use serde::Deserialize;
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

#[derive(Clone, Debug, Deserialize)]
struct Version {
    #[serde(rename = "Browser")]
    browser: String,
    #[serde(rename = "Protocol-Version")]
    protocol_version: String,
    #[serde(rename = "User-Agent")]
    user_agent: String,
    #[serde(rename = "V8-Version")]
    v8_version: String,
    #[serde(rename = "WebKit-Version")]
    webkit_version: String,
    #[serde(rename = "webSocketDebuggerUrl")]
    websocket_debugger_url: String,
}

struct Endpoint {
    sender: Writer<TcpStream>,
    call_id: CallId,
    pending: Arc<Mutex<HashMap<CallId, Sender<Message>>>>,
}

impl Endpoint {
    pub fn new(client: Client<TcpStream>) -> Result<Self, Error> {
        let (mut receiver, sender) = client.split()?;
        let pending = Arc::new(Mutex::new(HashMap::new()));

        thread::spawn(move || {
            for msg in receiver.incoming_messages() {
                match msg {
                    Ok(OwnedMessage::Text(text)) => {}
                    Ok(OwnedMessage::Close(close)) | Err(err) => {
                        break;
                    }
                    _ => {
                        // ignore message
                    }
                }
            }
        });

        Ok(Endpoint {
            sender,
            call_id: 0,
            pending,
        })
    }
}

impl CallSite for Endpoint {
    type Error = Error;

    fn call<T: Method>(&mut self, method: T) -> Result<T::ReturnObject, Self::Error> {
        self.call_id += 1;
        let call = method.to_method_call(self.call_id);
        let json = serde_json::to_string(&call)?;
        let msg = websocket::Message::text(json);

        self.sender.send_message(&msg)?;

        let (sender, receiver) = channel();

        self.pending.lock()?.insert(call.id(), sender);

        receiver.recv()?.try_into()
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
    let endpoint = Endpoint::new(client)?;

    Ok(())
}
