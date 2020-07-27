//! Handler implementation that binds lots of other stuff like SDL
//! This way, it's easier to separate out dependencies in the future if needed
use crate::Handler;
use crate::Scope;
use crate::Val;
use crate::DefaultHandler;
use std::sync::mpsc;

mod conv;

// const START_WINDOW_WIDTH: u32 = 800;
// const START_WINDOW_HEIGHT: u32 = 600;

#[allow(dead_code)]
pub struct OtherHandler {
    qtx: mpsc::Sender<Request>,
    srx: mpsc::Receiver<Response>,
}

impl OtherHandler {
    fn new(qtx: mpsc::Sender<Request>, srx: mpsc::Receiver<Response>) -> Self {
        Self { qtx, srx }
    }
}

impl Handler for OtherHandler {
    fn run(source_roots: Vec<String>, module_name: String) {
        run(source_roots, module_name, false);
    }

    fn test(source_roots: Vec<String>, module_name: String) {
        run(source_roots, module_name, true);
    }

    fn print(&mut self, _scope: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }
}

fn run(
    source_roots: Vec<String>,
    module_name: String,
    test: bool,
) {
    let (qtx, qrx) = mpsc::channel::<Request>();
    let (_stx, srx) = mpsc::channel::<Response>();
    let _handle = std::thread::Builder::new()
        .name("kb-main".to_owned())
        .spawn(move || {
            let handler = OtherHandler::new(qtx, srx);
            DefaultHandler::run_with_handler(handler, source_roots, module_name, test);
        })
        .unwrap();

    match qrx.recv() {
        Ok(_) => {}
        Err(mpsc::RecvError) => {}
    }

    _handle.join().unwrap();
}

enum Request {}

enum Response {}

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
