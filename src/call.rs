use serde::{de::DeserializeOwned, Serialize};

pub type CallId = usize;

pub trait CallSite {
    type Error;

    fn call<T: Method>(&mut self, method: T) -> Result<T::ReturnObject, Self::Error>;
}

#[cfg(feature = "async")]
pub trait AsyncCallSite {
    type Error;

    fn async_call<T: Method>(
        &mut self,
        method: T,
    ) -> futures::Future<Item = T::ReturnObject, Error = Self::Error>;
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
