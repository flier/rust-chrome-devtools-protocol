#![allow(deprecated)]

use std::error::Error;
use std::fmt;
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};
use std::str::FromStr;

use serde_json::{Map, Value};

use crate::CallId;

include!(concat!(env!("OUT_DIR"), "/protocol.rs"));

/// Represents a JSON `boolean` type.
pub type Boolean = bool;

/// Represents a JSON `integer` type.
pub type Integer = i64;

/// Represents a JSON `number` type.
pub type Number = f64;

/// Represents a JSON `object` type.
pub type Object = Map<String, Value>;

/// Represents `any` valid JSON value.
pub type Any = Value;

/// Represents a base64-encoded `binary` type.
#[derive(Clone, Debug, PartialEq)]
pub struct Binary(Vec<u8>);

impl Deref for Binary {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0.as_slice()
    }
}

impl DerefMut for Binary {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut_slice()
    }
}

impl From<Vec<u8>> for Binary {
    fn from(s: Vec<u8>) -> Self {
        Binary(s)
    }
}

impl Into<Vec<u8>> for Binary {
    fn into(self) -> Vec<u8> {
        self.0
    }
}

impl FromIterator<u8> for Binary {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = u8>,
    {
        Binary(iter.into_iter().collect())
    }
}

impl IntoIterator for Binary {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<u8>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromStr for Binary {
    type Err = base64::DecodeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        base64::decode(s).map(Binary)
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&base64::encode(&self.0))
    }
}

impl serde::Serialize for Binary {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> serde::Deserialize<'de> for Binary {
    fn deserialize<D>(deserializer: D) -> Result<Binary, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_str(BinaryVisitor)
    }
}

struct BinaryVisitor;

impl<'de> serde::de::Visitor<'de> for BinaryVisitor {
    type Value = Binary;

    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("a base64 encoded string")
    }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        s.parse().map_err(serde::de::Error::custom)
    }
}

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
        T: serde::de::DeserializeOwned,
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

#[cfg(test)]
mod tests {
    use serde_json::json;

    use crate::*;

    #[test]
    fn events() {
        let test_cases = vec![(
            Message::Event(Event::InspectorDetached(inspector::DetachedEvent {
                reason: "target_closed".to_owned(),
            })),
            json!({
                "method": "Inspector.detached",
                "params": {
                    "reason": "target_closed",
                }
            }),
        )];

        for (msg, json) in test_cases {
            assert_eq!(serde_json::to_string(&msg).unwrap(), json.to_string());
            assert_eq!(serde_json::from_value::<Message>(json).unwrap(), msg);
        }
    }
}
