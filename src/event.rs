pub trait EventSource<'a> {
    type Event;
    type Events: 'a + Iterator<Item = Self::Event>;

    fn events(&'a mut self) -> Self::Events;
}

#[cfg(feature = "async")]
pub trait AsyncEventSource {
    type Error;
    type Event;
    type Events: futures::Stream<Item = Self::Event, Error = Self::Error>;

    fn events(self) -> Self::Events;
}
