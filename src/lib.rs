mod browser;
mod call;
mod js;

#[doc(inline)]
pub use browser::{PROTOCOL_VERSION as BROWSER_PROTOCOL_VERSION, *};
#[doc(inline)]
pub use call::*;
#[doc(inline)]
pub use js::{PROTOCOL_VERSION as JS_PROTOCOL_VERSION, *};

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Object;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Any;
