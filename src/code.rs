use super::Mark;
use super::Var;
use super::VarScope;
use std::rc::Rc;
use std::fmt;

#[derive(Debug)]
pub enum Opcode {
    // Load constants
    Nil,
    Bool(bool),
    Number(f64),
    String(Rc<String>),
    NewList,
    NewFunc(Rc<Code>),

    // stack manipulation
    Pop,

    // variable access
    Get(VarScope, u32),
    Set(VarScope, u32),
    Tee(VarScope, u32),

    // operators
    Return,
    CallFunc(u32),
    Print,
    Binop(Binop),
    Unop(Unop),
}

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    // arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    TruncDivide,
    Remainder,

    // list
    Append,
}

#[derive(Debug, Clone, Copy)]
pub enum Unop {
    // arithmetic
    Negative,
    Positive,
}

pub struct Code {
    pub name: Rc<String>,
    pub nparams: usize,
    pub vars: Vec<Var>,
    pub ops: Vec<Opcode>,
    pub marks: Vec<Mark>,
}

impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Code({})", self.name)
    }
}

impl Code {
    pub fn add(&mut self, op: Opcode, mark: Mark) {
        self.ops.push(op);
        self.marks.push(mark);
        assert_eq!(self.ops.len(), self.marks.len());
    }
}
