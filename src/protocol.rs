#![allow(deprecated)]

use std::error::Error;
use std::fmt;
use std::str::FromStr;

use serde::de::DeserializeOwned;

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

impl FromStr for Message {
    type Err = serde_json::error::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_json::from_str::<Message>(s)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Response {
    #[serde(rename = "id")]
    pub call_id: CallId,
    pub result: Option<serde_json::Value>,
    pub error: Option<RemoteError>,
}

impl Response {
    pub fn into_result<T, E>(self) -> Result<T, E>
    where
        T: DeserializeOwned,
        E: From<RemoteError> + From<serde_json::Error>,
    {
        if let Some(err) = self.error {
            Err(err.into())
        } else {
            Ok(serde_json::from_value(self.result.unwrap())?)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RemoteError {
    pub code: i32,
    pub message: String,
}

impl fmt::Display for RemoteError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "remote error #{}, {}", self.code, self.message)
    }
}

impl Error for RemoteError {}
