use crate::RcStr;
use crate::Val;
use crate::rterr;
use std::convert::TryFrom;

pub(super) enum Request {
    Init(u32, u32),
}

impl TryFrom<Val> for Request {
    type Error = Val;

    fn try_from(val: Val) -> Result<Request, Val> {
        let arr = val.expect_list()?;
        if arr.borrow().is_empty() {
            return Err(rterr("Missing request type"));
        }
        let rtype = arr.borrow()[0].expect_number()? as u32;
        match rtype {
            // INIT
            0 => {
                let arr = arr.borrow();
                let width = getopt(arr.get(1), "GUI INIT: missing width")?.expect_number()?;
                let height = getopt(arr.get(2), "GUI INIT: missing height")?.expect_number()?;
                Ok(Request::Init(width as u32, height as u32))
            }
            req => Err(rterr(format!("Unrecognized request type: {:?}", req))),
        }
    }
}

fn getopt<T, S: Into<RcStr>>(opt: Option<T>, message: S) -> Result<T, Val> {
    match opt {
        Some(t) => Ok(t),
        None => Err(rterr(message)),
    }
}
