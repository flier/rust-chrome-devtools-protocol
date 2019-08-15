use std::fmt;

use serde::{de::DeserializeOwned, Serialize};

pub type CallId = usize;

pub trait CallSite {
    type Error;

    fn call<M>(&mut self, method: M) -> Result<M::ReturnObject, Self::Error>
    where
        M: Method,
        M::ReturnObject: 'static + Send;
}

#[cfg(feature = "async")]
pub trait AsyncCallSite {
    type Error;

    fn async_call<M>(
        self,
        method: M,
    ) -> Box<dyn futures::Future<Item = (M::ReturnObject, Self), Error = Self::Error> + Send>
    where
        M: Method,
        M::ReturnObject: 'static + Send;
}

#[derive(Serialize, Debug)]
pub struct Call<T> {
    id: CallId,
    #[serde(rename = "method")]
    method: &'static str,
    params: T,
}

impl<T> Call<T> {
    pub fn id(&self) -> CallId {
        self.id
    }

    pub fn method(&self) -> &'static str {
        self.method
    }

    pub fn params(&self) -> &T {
        &self.params
    }
}

pub trait Method: fmt::Debug + Serialize {
    const NAME: &'static str;

    type ReturnObject: DeserializeOwned;

    fn to_method_call(self, id: CallId) -> Call<Self>
    where
        Self: Sized,
    {
        Call {
            id,
            params: self,
            method: Self::NAME,
        }
    }
}
