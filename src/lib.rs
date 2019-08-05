mod browser;
mod js;

#[doc(inline)]
pub use browser::*;
#[doc(inline)]
pub use js::*;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Object;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Any;
