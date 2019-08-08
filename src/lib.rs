mod call;
mod protocol;

#[doc(inline)]
pub use call::*;
#[doc(inline)]
pub use protocol::*;

use serde_json::{Map, Value};

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
pub type Binary = Vec<u8>;
