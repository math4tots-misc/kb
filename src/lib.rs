//! A BASIC dialect

mod ast;
mod code;
mod handler;
mod lexer;
mod parser;
mod trans;
mod val;
mod vm;

pub use ast::*;
pub use code::*;
pub use handler::*;
pub use lexer::*;
pub use parser::*;
pub use trans::*;
pub use val::*;
pub use vm::*;

#[derive(Debug)]
pub struct BasicError {
    pub marks: Vec<Mark>,
    pub message: String,
}
