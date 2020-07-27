use crate::Val;
use crate::rterr;

pub(super) enum Response {
    Ok,
    Err(String),
}

impl Response {
    pub fn to_val(self) -> Result<Val, Val> {
        match self {
            Response::Ok => Ok(Val::Nil),
            Response::Err(message) => Err(rterr(message)),
        }
    }
}
