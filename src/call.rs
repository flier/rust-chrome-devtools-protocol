use std::fmt;

use serde::{de::DeserializeOwned, Serialize};

pub type CallId = usize;

#[derive(Serialize, Debug)]
pub struct MethodCall<T>
where
    T: fmt::Debug,
{
    #[serde(rename = "method")]
    method_name: &'static str,
    pub id: CallId,
    params: T,
}

impl<T> MethodCall<T>
where
    T: fmt::Debug,
{
    pub fn get_params(&self) -> &T {
        &self.params
    }
}

pub trait Method: fmt::Debug {
    const NAME: &'static str;

    type ReturnObject: DeserializeOwned + fmt::Debug;

    fn into_method_call(self, call_id: CallId) -> MethodCall<Self>
    where
        Self: Sized,
    {
        MethodCall {
            id: call_id,
            params: self,
            method_name: Self::NAME,
        }
    }
}
