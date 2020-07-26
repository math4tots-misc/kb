//! Handler implementation that binds lots of other stuff like SDL
//! This way, it's easier to separate out dependencies in the future if needed
use crate::rterr;
use crate::Color;
use crate::Event;
use crate::Handler;
use crate::RcStr;
use crate::Scope;
use crate::Val;
use crate::VideoHandler;
use sdl2::event::Event as SdlEvent;
use sdl2::render::WindowCanvas;
use sdl2::EventPump;
use sdl2::Sdl;
use sdl2::VideoSubsystem;

mod conv;

const START_WINDOW_WIDTH: u32 = 800;
const START_WINDOW_HEIGHT: u32 = 600;

pub struct OtherHandler {
    sdl: Option<Sdl>,
    video: Option<VideoSubsystem>,
    events: Option<EventPump>,

    window: Option<WindowCanvas>,
}

impl OtherHandler {
    pub fn new() -> Self {
        Self {
            sdl: None,
            video: None,
            events: None,
            window: None,
        }
    }
    pub fn sdl(&mut self) -> Result<&mut Sdl, Val> {
        if self.sdl.is_none() {
            match sdl2::init() {
                Ok(sdl) => {
                    self.sdl = Some(sdl);
                }
                Err(message) => {
                    return Err(rterr(message));
                }
            }
        }
        Ok(self.sdl.as_mut().unwrap())
    }
    pub fn events(&mut self) -> Result<&mut EventPump, Val> {
        if self.events.is_none() {
            self.events = Some(stoerr(self.sdl()?.event_pump())?);
        }
        Ok(self.events.as_mut().unwrap())
    }
    pub fn video(&mut self) -> Result<&mut VideoSubsystem, Val> {
        if self.video.is_none() {
            self.video = Some(stoerr(self.sdl()?.video())?);
        }
        Ok(self.video.as_mut().unwrap())
    }
    pub fn window(&mut self) -> Result<&mut WindowCanvas, Val> {
        if self.window.is_none() {
            let window = etoerr(
                self.video()?
                    .window("kb", START_WINDOW_WIDTH, START_WINDOW_HEIGHT)
                    .build(),
            )?;
            let canvas = etoerr(window.into_canvas().build())?;
            self.window = Some(canvas);
        }
        Ok(self.window.as_mut().unwrap())
    }
}

impl Handler for OtherHandler {
    fn poll(&mut self) -> Result<Vec<Event>, Val> {
        let mut events = Vec::new();
        for event in self.events()?.poll_iter() {
            match event {
                SdlEvent::Quit { timestamp: _ } => {
                    // TODO: In the future make this configurable
                    // For now, we always throw on a quit event, so that
                    // there's a way to exit the application even if you forget
                    return Err(rterr("Quit"));
                }
                SdlEvent::TextInput {
                    timestamp: _,
                    window_id: _,
                    text,
                } => {
                    events.push(Event::Text(text));
                }
                _ => {}
            }
        }
        Ok(events)
    }
    fn print(&mut self, _scope: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }
    fn video(&mut self) -> Result<&mut dyn VideoHandler, Val> {
        self.window()?;
        Ok(self)
    }
}

impl VideoHandler for OtherHandler {
    fn set_color(&mut self, color: Color) -> Result<(), Val> {
        let sdlc: sdl2::pixels::Color = color.into();
        self.window()?.set_draw_color(sdlc);
        Ok(())
    }
    fn clear(&mut self) -> Result<(), Val> {
        self.window()?.clear();
        Ok(())
    }
    fn present(&mut self) -> Result<(), Val> {
        self.window()?.present();
        Ok(())
    }
}

fn stoerr<T, S: Into<RcStr>>(r: Result<T, S>) -> Result<T, Val> {
    match r {
        Ok(t) => Ok(t),
        Err(s) => Err(rterr(s.into())),
    }
}

fn etoerr<T, E: std::error::Error>(r: Result<T, E>) -> Result<T, Val> {
    match r {
        Ok(t) => Ok(t),
        Err(e) => Err(rterr(format!("{:?}", e))),
    }
}
