use super::rterr;
use super::Color;
use super::Event;
use super::Scope;
use super::Val;

/// Interface to the outside world
pub trait Handler {
    fn print(&mut self, scope: &mut Scope, val: Val) -> Result<(), Val>;
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
}

pub struct DefaultHandler;

impl Handler for DefaultHandler {
    fn print(&mut self, _: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }
}
