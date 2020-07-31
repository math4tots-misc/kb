use super::ArgSpec;
use super::Binop;
use super::RcStr;
use super::Tenop;
use super::Unop;
use super::VarScope;
use super::Zop;
use std::fmt;
use std::fmt::Write;
use std::rc::Rc;

pub struct Source {
    pub name: RcStr,
    pub data: RcStr,
}

impl fmt::Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Source({})", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct Mark {
    pub source: Rc<Source>,
    pub pos: usize,
}

impl Mark {
    pub fn format(&self) -> String {
        let mut ret = String::new();
        let out = &mut ret;
        writeln!(out, "in {:?} on line {}", self.source.name, self.lineno()).unwrap();
        let start = self.source.data[..self.pos]
            .rfind('\n')
            .map(|x| x + 1)
            .unwrap_or(0);
        let end = self.source.data[self.pos..]
            .find('\n')
            .map(|x| x + self.pos)
            .unwrap_or(self.source.data.len());
        writeln!(out, "{}", &self.source.data[start..end]).unwrap();
        for _ in start..self.pos {
            write!(out, " ").unwrap();
        }
        writeln!(out, "*").unwrap();
        ret
    }
    pub fn lineno(&self) -> usize {
        self.source.data[..self.pos].matches('\n').count() + 1
    }
}

pub struct File {
    pub source: Rc<Source>,
    pub imports: Vec<Import>,
    pub funcs: Vec<FuncDisplay>,
    pub clss: Vec<ClassDisplay>,
    pub body: Stmt,

    // annotated data
    pub vars: Vec<Var>,
}

impl File {
    pub fn name(&self) -> &RcStr {
        &self.source.name
    }
}

#[derive(Debug, Clone)]
pub struct Var {
    pub mark: Mark,
    pub name: RcStr, // unique in the scope it is declared
    pub vscope: VarScope,
    pub index: u32,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub mark: Mark,
    pub module_name: RcStr,
    pub alias: RcStr,

    // annotated data
    pub unique_name: RcStr,
}

pub struct ClassDisplay {
    pub mark: Mark,
    pub short_name: RcStr,
    pub bases: Vec<Expr>,

    // list of method names
    // the corresponding functions are deduced later based on the
    // class's full name
    pub methods: Vec<RcStr>,

    // annotated data
    pub as_var: Option<Var>,
}

pub struct FuncDisplay {
    pub mark: Mark,
    pub generator: bool,
    pub test: bool,
    pub short_name: RcStr,
    pub argspec: ArgSpec,
    pub body: Stmt,

    // annotated data
    pub vars: Vec<Var>,
    pub as_var: Option<Var>,
}

impl FuncDisplay {
    pub fn full_name(&self) -> &RcStr {
        &self.as_var.as_ref().unwrap().name
    }
}

pub struct Stmt {
    pub mark: Mark,
    pub desc: StmtDesc,
}

pub enum StmtDesc {
    Block(Vec<Stmt>),
    Return(Option<Expr>),
    Assign(AssignTarget, Vec<AssignTarget>, Expr),
    Expr(Expr),
    Print(Expr),
    Assert(Expr),
    AssertThrow(Box<Stmt>),

    // Control flow
    Label(RcStr),
    Goto(RcStr),
    If(Vec<(Expr, Stmt)>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    ForIn(AssignTarget, Expr, Box<Stmt>),
    ForClassic(AssignTarget, Expr, Expr, bool, f64, Box<Stmt>),
    Try(Box<Stmt>, AssignTarget, Box<Stmt>),
    Throw(Expr),
}

#[derive(Debug)]
pub struct AssignTarget {
    pub mark: Mark,
    pub desc: AssignTargetDesc,
}

#[derive(Debug)]
pub enum AssignTargetDesc {
    Name(RcStr),
    List(Vec<AssignTarget>, Option<Box<AssignTarget>>),
    Subscript(Expr, Expr),
    Attribute(Expr, RcStr),
}

#[derive(Debug)]
pub struct Expr {
    pub mark: Mark,
    pub desc: ExprDesc,
}

#[derive(Debug)]
pub enum ExprDesc {
    Nil,
    Bool(bool),
    Number(f64),
    String(RcStr),
    List(Vec<Expr>, Option<Box<Expr>>),
    Set(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),

    ListComprehension(Box<Expr>, Box<AssignTarget>, Box<Expr>, Option<Box<Expr>>),

    GetVar(RcStr),
    GetAttr(Box<Expr>, RcStr),

    CallFunc(Box<Expr>, Vec<Expr>, Vec<(RcStr, Expr)>),
    CallMethod(Box<Expr>, RcStr, Vec<Expr>, Vec<(RcStr, Expr)>),

    Tenop(Tenop, Box<Expr>, Box<Expr>, Box<Expr>),
    Binop(Binop, Box<Expr>, Box<Expr>),
    Unop(Unop, Box<Expr>),
    Zop(Zop),
    Send(Box<Expr>, Vec<Expr>),

    // Logical operators
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),

    Yield(Box<Expr>),
    Next(Box<Expr>), // gets [next-or-nil, has_next] from a generator

    Cat(Vec<Expr>),

    // Gets the disassembly of a function as a string
    Disasm(Box<Expr>),
}
