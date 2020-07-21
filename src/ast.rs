use super::Binop;
use super::Unop;
use super::VarScope;
use std::fmt;
use std::fmt::Write;
use std::rc::Rc;

pub struct Source {
    pub name: Rc<String>,
    pub data: Rc<String>,
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
        writeln!(out, "on line {}", self.lineno()).unwrap();
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
    pub body: Stmt,

    // annotated data
    pub vars: Vec<Var>,
}

impl File {
    pub fn name(&self) -> &Rc<String> {
        &self.source.name
    }
}

#[derive(Debug, Clone)]
pub struct Var {
    pub mark: Mark,
    pub name: Rc<String>, // unique in the scope it is declared
    pub vscope: VarScope,
    pub index: u32,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub mark: Mark,
    pub module_name: Rc<String>,
    pub alias: Rc<String>,

    // annotated data
    pub unique_name: Rc<String>,
}

pub struct FuncDisplay {
    pub mark: Mark,
    pub short_name: Rc<String>,
    pub params: Vec<Rc<String>>,
    pub body: Stmt,

    // annotated data
    pub vars: Vec<Var>,
    pub as_var: Option<Var>,
}

impl FuncDisplay {
    pub fn full_name(&self) -> &Rc<String> {
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
    DeclVar(Rc<String>, Expr),
    Expr(Expr),
    Print(Expr),

    // Control flow
    Label(Rc<String>),
    Goto(Rc<String>),
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
    String(Rc<String>),
    List(Vec<Expr>),

    GetVar(Rc<String>),
    SetVar(Rc<String>, Box<Expr>),
    GetAttr(Box<Expr>, Rc<String>),

    CallFunc(Box<Expr>, Vec<Expr>),

    Binop(Binop, Box<Expr>, Box<Expr>),
    Unop(Unop, Box<Expr>),
}
