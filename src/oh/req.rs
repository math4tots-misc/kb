use crate::rterr;
use crate::RcStr;
use crate::Val;
use std::convert::TryFrom;

pub(super) enum Request {
    Quit,
    Init(u32, u32),
    Poll,
}

impl TryFrom<Val> for Request {
    type Error = Val;

    fn try_from(val: Val) -> Result<Request, Val> {
        let arr = val.expect_list()?;
        let arr = arr.borrow();
        if arr.is_empty() {
            return Err(rterr("Missing request type"));
        }
        let rtype = arr[0].expect_number()? as u32;
        match rtype {
            // Init
            0 => {
                checkargc("Init", arr.len(), 2)?;
                let width = getopt(arr.get(1), "GUI INIT: missing width")?.expect_number()?;
                let height = getopt(arr.get(2), "GUI INIT: missing height")?.expect_number()?;
                Ok(Request::Init(width as u32, height as u32))
            }
            // Poll
            1 => {
                checkargc("Poll", arr.len(), 0)?;
                Ok(Request::Poll)
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

fn checkargc(fname: &str, arrlen: usize, expc: usize) -> Result<(), Val> {
    let argc = arrlen - 1;
    if argc != expc {
        Err(rterr(format!(
            "GUI/{:?} expects {} args but got {}",
            fname, expc, argc
        )))
    } else {
        Ok(())
    }
}
