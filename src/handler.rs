use super::Scope;
use super::Val;

/// Interface to the outside world
pub trait Handler {
    fn print(&mut self, scope: &mut Scope, val: Val) -> Result<(), Val>;
}

pub struct DefaultHandler;

impl Handler for DefaultHandler {
    fn print(&mut self, _: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }
}
