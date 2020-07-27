use crate::rterr;
use crate::Event;
use crate::Val;

pub(super) enum Response {
    Ok,
    Err(String),

    /// In response to a 'Poll' request
    Events(Vec<Event>),
}

impl Response {
    pub fn to_val(self) -> Result<Val, Val> {
        match self {
            Response::Ok => Ok(Val::Nil),
            Response::Err(message) => Err(rterr(message)),
            Response::Events(events) => Ok(events
                .into_iter()
                .map(|event| Val::from(event))
                .collect::<Vec<_>>()
                .into()),
        }
    }
}
