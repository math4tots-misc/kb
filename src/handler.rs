use super::Scope;
use super::Val;

/// Interface to the outside world
pub trait Handler {
    /// Behavior on 'print' statements
    fn print(&mut self, _: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }

    /// Returns seconds since UNIX epoch as float
    fn time(&mut self) -> f64 {
        std::time::UNIX_EPOCH.elapsed().unwrap().as_secs_f64()
    }
}

pub struct DefaultHandler;

impl Handler for DefaultHandler {
    fn print(&mut self, _: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }
}
