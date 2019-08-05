mod browser;
mod call;
mod js;

#[doc(inline)]
pub use browser::*;
#[doc(inline)]
pub use call::*;
#[doc(inline)]
pub use js::*;

use serde::{Deserialize, Serialize};

/// The current version of `browser` protocol.
pub const BROWSER_PROTOCOL_VERSION: &str = browser::PROTOCOL_VERSION;
/// The current version of `javascript` protocol.
pub const JS_PROTOCOL_VERSION: &str = js::PROTOCOL_VERSION;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Object;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Any;
