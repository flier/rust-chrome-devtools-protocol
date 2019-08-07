#![allow(deprecated)]

use std::str::FromStr;

use crate::CallId;

include!(concat!(env!("OUT_DIR"), "/protocol.rs"));

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
#[allow(clippy::large_enum_variant)]
pub enum Message {
    Event(Event),
    Response(Response),
    ConnectionShutdown,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Response {
    #[serde(rename = "id")]
    pub call_id: CallId,
    pub result: Option<serde_json::Value>,
    pub error: Option<RemoteError>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RemoteError {
    pub code: i32,
    pub message: String,
}

impl FromStr for Message {
    type Err = serde_json::error::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_json::from_str::<Message>(s)
    }
}
