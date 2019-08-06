use serde::{de::DeserializeOwned, Serialize};

pub type CallId = usize;

#[derive(Serialize, Debug)]
pub struct Call<T> {
    pub id: CallId,
    #[serde(rename = "method")]
    method_name: &'static str,
    params: T,
}

impl<T> Call<T> {
    pub fn name(&self) -> &'static str {
        self.method_name
    }

    pub fn params(&self) -> &T {
        &self.params
    }
}

pub trait Method {
    const NAME: &'static str;

    type ReturnObject: DeserializeOwned;

    fn call(self, id: CallId) -> Call<Self>
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
