//! A BASIC dialect

mod ast;
mod code;
mod handler;
mod lexer;
mod loader;
mod parser;
mod trans;
mod val;
mod vm;

pub use ast::*;
pub use code::*;
pub use handler::*;
pub use lexer::*;
pub use loader::*;
pub use parser::*;
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
