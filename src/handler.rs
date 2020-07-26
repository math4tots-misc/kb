use super::rterr;
use super::Color;
use super::Event;
use super::Scope;
use super::Val;

/// Interface to the outside world
pub trait Handler {
    fn print(&mut self, scope: &mut Scope, val: Val) -> Result<(), Val>;

    /// Returns seconds since UNIX epoch as float
    fn time(&mut self) -> f64 {
        std::time::UNIX_EPOCH.elapsed().unwrap().as_secs_f64()
    }

    fn video(&mut self) -> Result<&mut dyn VideoHandler, Val> {
        Err(rterr("Video mode not supported in this environment"))
    }
    fn poll(&mut self) -> Result<Vec<Event>, Val> {
        Err(rterr("Event polling is not supported in this environment"))
    }
}

pub trait VideoHandler {
    fn set_color(&mut self, color: Color) -> Result<(), Val>;
    fn clear(&mut self) -> Result<(), Val>;
    fn present(&mut self) -> Result<(), Val>;
    fn draw_pixel(&mut self, x: i32, y: i32) -> Result<(), Val>;

    /// returns (width, height) pair of the drawing dimensions of the screen
    fn dim(&mut self) -> Result<(u32, u32), Val>;
}

pub struct DefaultHandler;

impl Handler for DefaultHandler {
    fn print(&mut self, _: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }
}
