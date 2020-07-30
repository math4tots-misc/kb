use crate::rterr;
use crate::RcStr;
use crate::Val;
use a2d::Color;
use std::collections::HashMap;
use std::convert::TryFrom;

pub(super) enum Request {
    Quit,
    Init(u32, u32),
    Poll,
    Flush(Option<HashMap<(u32, u32), Color>>),
    SetPixel(u32, u32, Color),
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
            32 => {
                checkargc("Flush", arr.len(), 0)?;
                Ok(Request::Flush(None))
            }
            // Set pixel
            33 => {
                checkargc("SetPixel", arr.len(), 3)?;
                let x = getopt(arr.get(1), "GUI SET_PIXEL: missing x")?.expect_number()? as u32;
                let y = getopt(arr.get(2), "GUI SET_PIXEL: missing y")?.expect_number()? as u32;
                let color = getopt(arr.get(3), "GUI SET_PIXEL: missing color")?.to_color()?;
                Ok(Request::SetPixel(x, y, color))
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
