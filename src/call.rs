use serde::{de::DeserializeOwned, Serialize};

#[cfg(feature = "async")]
use futures::future::Future;

pub type CallId = usize;

pub trait CallSite {
    type Error;

    fn call<M: Method>(&mut self, method: M) -> Result<M::ReturnObject, Self::Error>;
}

#[cfg(feature = "async")]
pub trait AsyncCallSite {
    type Error;

    fn async_call<M: Method>(
        &mut self,
        method: M,
    ) -> Box<dyn futures::Future<Item = M::ReturnObject, Error = <Self as AsyncCallSite>::Error>>;
}

#[cfg(feature = "async")]
impl<T> CallSite for T
where
    T: AsyncCallSite,
{
    type Error = <T as AsyncCallSite>::Error;

    fn call<M: Method>(&mut self, method: M) -> Result<M::ReturnObject, Self::Error> {
        self.async_call(method).wait()
    }
}

#[derive(Serialize, Debug)]
pub struct Call<T> {
    id: CallId,
    #[serde(rename = "method")]
    method_name: &'static str,
    params: T,
}

impl<T> Call<T> {
    pub fn id(&self) -> CallId {
        self.id
    }

    pub fn name(&self) -> &'static str {
        self.method_name
    }

    pub fn params(&self) -> &T {
        &self.params
    }
}

pub trait Method: Serialize {
    const NAME: &'static str;

    type ReturnObject: DeserializeOwned;

    fn to_method_call(self, id: CallId) -> Call<Self>
    where
        Self: Sized,
    {
        Call {
            id,
            params: self,
            method_name: Self::NAME,
        }
    }
}
