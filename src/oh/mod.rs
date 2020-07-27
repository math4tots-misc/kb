//! Handler implementation that binds lots of other stuff like SDL
//! This way, it's easier to separate out dependencies in the future if needed
use crate::DefaultHandler;
use crate::Handler;
use crate::Scope;
use crate::Val;
use crate::rterr;
use std::sync::mpsc;
use winit::event::Event as WinitEvent;
use winit::event::WindowEvent;
use winit::event_loop::ControlFlow;
use winit::event_loop::EventLoop;
use winit::window::WindowBuilder;
use std::convert::TryFrom;

mod conv;
mod req;
mod res;

use req::*;
use res::*;

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

    fn gui_send(&mut self, message: Val) -> Result<Val, Val> {
        match self.qtx.send(Request::try_from(message)?) {
            Ok(()) => {}
            Err(error) => return Err(rterr(format!("{:?}", error))),
        }
        match self.srx.recv() {
            Ok(resp) => resp.to_val(),
            Err(error) => return Err(rterr(format!("{:?}", error))),
        }
    }
}

fn run(source_roots: Vec<String>, module_name: String, test: bool) {
    let (qtx, qrx) = mpsc::channel::<Request>();
    let (stx, srx) = mpsc::channel::<Response>();
    let _handle = std::thread::Builder::new()
        .name("kb-main".to_owned())
        .spawn(move || {
            let handler = OtherHandler::new(qtx, srx);
            DefaultHandler::run_with_handler(handler, source_roots, module_name, test);
        })
        .unwrap();

    match qrx.recv() {
        Ok(Request::Init(width, height)) => {
            let event_loop = EventLoop::<Request>::with_user_event();
            let proxy = event_loop.create_proxy();
            std::thread::Builder::new()
                .name("kb-gui-event-pipe".to_owned())
                .spawn(move || loop {
                    match qrx.recv() {
                        Ok(request) => match proxy.send_event(request) {
                            Ok(()) => {}
                            Err(_) => break,
                        },
                        Err(mpsc::RecvError) => break,
                    }
                })
                .unwrap();
            let _window = WindowBuilder::new()
                .with_inner_size(winit::dpi::LogicalSize { width, height })
                .build(&event_loop)
                .unwrap();
            stx.send(Response::Ok).unwrap();
            event_loop.run(|event, _, control_flow| match event {
                WinitEvent::WindowEvent {
                    event,
                    window_id: _,
                } => match event {
                    WindowEvent::CloseRequested => {
                        *control_flow = ControlFlow::Exit;
                    }
                    _ => {}
                },
                _ => {}
            });
        }

        // GUI mode was never requested
        Err(mpsc::RecvError) => {}
    }

    _handle.join().unwrap();
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
