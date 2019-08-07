mod protocol {
    #![allow(deprecated)]

    include!(concat!(env!("OUT_DIR"), "/protocol.rs"));
}
mod call;

#[doc(inline)]
pub use call::*;
#[doc(inline)]
pub use protocol::*;

use serde_json::{Map, Value};

/// Represents a JSON key/value object.
pub type Object = Map<String, Value>;

/// Represents any valid JSON value.
pub type Any = Value;
