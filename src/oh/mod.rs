//! Handler implementation that binds lots of other stuff like SDL
//! This way, it's easier to separate out dependencies in the future if needed
use crate::Handler;
use crate::Scope;
use crate::Val;

mod conv;

// const START_WINDOW_WIDTH: u32 = 800;
// const START_WINDOW_HEIGHT: u32 = 600;

pub struct OtherHandler {
}

impl OtherHandler {
    pub fn new() -> Self {
        Self {}
    }
}

impl Handler for OtherHandler {
    fn print(&mut self, _scope: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }
}

// fn stoerr<T, S: Into<RcStr>>(r: Result<T, S>) -> Result<T, Val> {
//     match r {
//         Ok(t) => Ok(t),
//         Err(s) => Err(rterr(s.into())),
//     }
// }

// fn etoerr<T, E: std::error::Error>(r: Result<T, E>) -> Result<T, Val> {
//     match r {
//         Ok(t) => Ok(t),
//         Err(e) => Err(rterr(format!("{:?}", e))),
//     }
// }
