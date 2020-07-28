//! Handler implementation that binds lots of other stuff like SDL
//! This way, it's easier to separate out dependencies in the future if needed
use crate::rterr;
use crate::DefaultHandler;
use crate::Event;
use crate::Handler;
use crate::Scope;
use crate::Val;
use std::convert::TryFrom;
use std::sync::mpsc;
use winit::dpi::LogicalPosition;
use winit::dpi::LogicalSize;
use winit::event::ElementState;
use winit::event::Event as WinitEvent;
use winit::event::WindowEvent;
use winit::event_loop::ControlFlow;
use winit::event_loop::EventLoop;
use winit::window::WindowBuilder;

mod conv;
mod ebuf;
mod req;
mod res;

use ebuf::*;
use req::*;
use res::*;

// const START_WINDOW_WIDTH: u32 = 800;
// const START_WINDOW_HEIGHT: u32 = 600;

const EVENTS_BUFFER_SIZE: usize = 8;

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
            let handler =
                DefaultHandler::run_with_handler(handler, source_roots, module_name, test)
                    .into_handler();
            handler.qtx.send(Request::Quit).unwrap();
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
            let window = WindowBuilder::new()
                .with_inner_size(LogicalSize { width, height })
                // .with_resizable(false)
                .build(&event_loop)
                .unwrap();
            stx.send(Response::Ok).unwrap();
            let mut events = EventBuffer::new(EVENTS_BUFFER_SIZE);
            let mut cursor_pos: LogicalPosition<f64> = (0.0, 0.0).into();
            event_loop.run(move |event, _, control_flow| match event {
                WinitEvent::WindowEvent {
                    event,
                    window_id: _,
                } => match event {
                    WindowEvent::CloseRequested => {
                        *control_flow = ControlFlow::Exit;
                    }
                    WindowEvent::KeyboardInput {
                        device_id: _,
                        input,
                        is_synthetic: _,
                    } => {
                        if let Some(keycode) = input.virtual_keycode {
                            events.push(Event::Key(format!("{:?}", keycode)));
                        }
                    }
                    WindowEvent::MouseInput {
                        device_id: _,
                        state,
                        button,
                        ..
                    } => {
                        let x = cursor_pos.x;
                        let y = cursor_pos.y;
                        match state {
                            ElementState::Pressed => {
                                events.push(Event::MouseDown(format!("{:?}", button), x, y));
                            }
                            ElementState::Released => {
                                events.push(Event::MouseUp(format!("{:?}", button), x, y));
                            }
                        }
                    }
                    WindowEvent::CursorMoved {
                        device_id: _,
                        position,
                        ..
                    } => {
                        cursor_pos =
                            LogicalPosition::from_physical(position, window.scale_factor());
                    }
                    _ => {}
                },
                WinitEvent::UserEvent(request) => {
                    if let Request::Quit = request {
                        *control_flow = ControlFlow::Exit;
                    } else {
                        stx.send(match request {
                            Request::Quit => panic!("Impossible Quit"),
                            Request::Init(..) => {
                                Response::Err("GUI already initialized".to_owned())
                            }
                            Request::Poll => Response::Events(events.clear()),
                        })
                        .unwrap();
                    }
                }
                _ => {}
            });
        }

        Ok(_) => stx
            .send(Response::Err(format!(
                "GUI request made before being initialized"
            )))
            .unwrap(),

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
