use std::iter::Flatten;

use futures::{stream::Wait, Stream};

pub trait EventSource<'a> {
    type Event;
    type Events: 'a + Iterator<Item = Self::Event>;

    fn events(&'a self) -> Self::Events;
}

#[cfg(feature = "async")]
pub trait AsyncEventSource<'a> {
    type Error;
    type Event;
    type Events: 'a + Stream<Item = Self::Event, Error = Self::Error>;

    fn events(&'a self) -> Self::Events;
}

#[cfg(feature = "async")]
impl<'a, T> EventSource<'a> for T
where
    T: AsyncEventSource<'a>,
{
    type Event = <T as AsyncEventSource<'a>>::Event;
    type Events = Flatten<Wait<<T as AsyncEventSource<'a>>::Events>>;

    fn events(&'a self) -> Self::Events {
        AsyncEventSource::events(self).wait().flatten()
    }
}
