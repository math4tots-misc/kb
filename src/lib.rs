//! A BASIC dialect

mod ast;
mod code;
mod handler;
mod lexer;
mod loader;
mod m;
mod parser;
mod rcstr;
mod trans;
mod val;
mod vm;

pub use ast::*;
pub use code::*;
pub use handler::*;
pub use lexer::*;
pub use loader::*;
pub use m::*;
pub use parser::*;
pub use rcstr::*;
pub use trans::*;
pub use val::*;
pub use vm::*;

#[derive(Debug)]
pub struct BasicError {
    pub marks: Vec<Mark>,
    pub message: String,
}

impl From<std::io::Error> for BasicError {
    fn from(e: std::io::Error) -> Self {
        Self {
            marks: vec![],
            message: format!("{:?}", e),
        }
    }
}

impl BasicError {
    pub fn format(&self) -> String {
        use std::fmt::Write;
        let mut ret = String::new();
        let out = &mut ret;
        for mark in &self.marks {
            write!(out, "{}", mark.format()).unwrap();
        }
        writeln!(out, "{}", self.message).unwrap();
        ret
    }
}
