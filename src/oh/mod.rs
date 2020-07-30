//! Handler implementation that binds lots of other stuff like SDL
//! This way, it's easier to separate out dependencies in the future if needed
use crate::rterr;
use crate::DefaultHandler;
use crate::VideoHandler;
use crate::Event;
use crate::Handler;
use crate::Scope;
use crate::Val;
use std::sync::mpsc;
use std::collections::HashMap;
use winit::dpi::LogicalPosition;
use winit::dpi::LogicalSize;
use winit::event::ElementState;
use winit::event::Event as WinitEvent;
use winit::event::WindowEvent;
use winit::event_loop::ControlFlow;
use winit::event_loop::EventLoop;
use winit::window::WindowBuilder;
use a2d::Color;

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

    pending_pixel_writes: HashMap<(u32, u32), Color>,
}

impl OtherHandler {
    fn new(qtx: mpsc::Sender<Request>, srx: mpsc::Receiver<Response>) -> Self {
        Self { qtx, srx, pending_pixel_writes: HashMap::new() }
    }

    fn send(&mut self, req: Request) -> Result<Response, Val> {
        match self.qtx.send(req) {
            Ok(()) => {}
            Err(error) => return Err(rterr(format!("{:?}", error))),
        }
        match self.srx.recv() {
            Ok(resp) => Ok(resp),
            Err(error) => return Err(rterr(format!("{:?}", error))),
        }
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

    fn init_video(&mut self, width: u32, height: u32) -> Result<(), Val> {
        match self.send(Request::Init(width, height)) {
            Ok(_response) => Ok(()),
            Err(error) => Err(error),
        }
    }

    fn video(&mut self) -> Result<&mut dyn VideoHandler, Val> {
        Ok(self)
    }
}

impl VideoHandler for OtherHandler {
    fn flush(&mut self) -> Result<(), Val> {
        let pixel_writes = if !self.pending_pixel_writes.is_empty() {
            Some(std::mem::replace(&mut self.pending_pixel_writes, HashMap::new()))
        } else {
            None
        };
        self.send(Request::Flush(pixel_writes))?;
        Ok(())
    }

    fn poll(&mut self) -> Result<Vec<Event>, Val> {
        match self.send(Request::Poll) {
            Ok(Response::Events(events)) => Ok(events),
            Ok(resp) => panic!("Invalid response: {:?}", resp),
            Err(error) => Err(error),
        }
    }

    fn set_pixel(&mut self, x: u32, y: u32, color: Color) -> Result<(), Val> {
        self.pending_pixel_writes.insert((x, y), color);
        Ok(())
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
                .with_resizable(false)
                .build(&event_loop)
                .unwrap();
            let mut graphics: a2d::Graphics2D =
                futures::executor::block_on(a2d::Graphics2D::new(width, height, &window)).unwrap();
            stx.send(Response::Ok).unwrap();
            let mut events = EventBuffer::new(EVENTS_BUFFER_SIZE);
            let mut cursor_pos: LogicalPosition<f64> = (0.0, 0.0).into();
            event_loop.run(move |event, _, control_flow| {
                *control_flow = ControlFlow::Wait;
                match event {
                    WinitEvent::RedrawRequested(_) => {
                        graphics.force_render().unwrap();
                    }
                    WinitEvent::MainEventsCleared => {
                        graphics.render_if_dirty().unwrap();
                    }
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
                                Request::Flush(pixel_writes) => {
                                    if let Some(pixel_writes) = pixel_writes {
                                        for ((x, y), color) in pixel_writes {
                                            graphics.set_pixel(x as usize, y as usize, color).unwrap();
                                        }
                                    }
                                    match graphics.flush() {
                                        Ok(()) => Response::Ok,
                                        Err(error) => Response::Err(format!("{:?}", error)),
                                    }
                                }
                                Request::SetPixel(x, y, color) => {
                                    match graphics.set_pixel(x as usize, y as usize, color) {
                                        Ok(()) => Response::Ok,
                                        Err(error) => Response::Err(format!("{:?}", error)),
                                    }
                                }
                            })
                            .unwrap();
                        }
                    }
                    _ => {}
                }
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
