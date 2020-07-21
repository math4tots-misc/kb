use super::Mark;
use super::Var;
use super::VarScope;
use std::fmt;
use std::rc::Rc;

pub const INVALID_LABEL_LOC: usize = usize::MAX;

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

    // control flow
    Label(u32),
    Goto(u32),
    GotoIfFalse(u32),
    GotoIfFalseNoPop(u32),

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

    // comparison
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

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
    name: Rc<String>,
    nparams: usize,
    vars: Vec<Var>,
    ops: Vec<Opcode>,
    marks: Vec<Mark>,
    label_map: Vec<usize>,
    label_names: Vec<Rc<String>>,
}

impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Code({})", self.name)
    }
}

impl Code {
    pub fn new(name: Rc<String>, nparams: usize, vars: Vec<Var>) -> Self {
        Self {
            name,
            nparams,
            vars,
            ops: vec![],
            marks: vec![],
            label_map: vec![],
            label_names: vec![],
        }
    }
    pub fn add(&mut self, op: Opcode, mark: Mark) {
        self.ops.push(op);
        self.marks.push(mark);
        assert_eq!(self.ops.len(), self.marks.len());
    }
    pub fn set_label_data(&mut self, map: Vec<usize>, names: Vec<Rc<String>>) {
        self.label_map = map;
        self.label_names = names;
    }
    pub fn label_map(&self) -> &Vec<usize> {
        &self.label_map
    }
    pub fn label_names(&self) -> &Vec<Rc<String>> {
        &self.label_names
    }
    pub fn len(&self) -> usize {
        self.ops.len()
    }
    pub fn name(&self) -> &Rc<String> {
        &self.name
    }
    pub fn nparams(&self) -> usize {
        self.nparams
    }
    pub fn vars(&self) -> &Vec<Var> {
        &self.vars
    }
    pub fn ops(&self) -> &Vec<Opcode> {
        &self.ops
    }
    pub fn ops_mut(&mut self) -> &mut Vec<Opcode> {
        &mut self.ops
    }
    pub fn marks(&self) -> &Vec<Mark> {
        &self.marks
    }
    pub fn fetch(&self, i: usize) -> &Opcode {
        &self.ops[i]
    }
}
