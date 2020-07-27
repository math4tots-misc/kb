use crate::Val;

pub(super) enum Response {
    Ok,
}

impl Response {
    pub fn to_val(self) -> Result<Val, Val> {
        match self {
            Response::Ok => Ok(Val::Nil),
        }
    }
}
