use super::BasicError;
use super::Mark;
use super::RcStr;
use super::Val;
use super::Var;
use super::VarScope;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub enum Opcode {
    // Load constants
    Nil,
    Bool(bool),
    Number(f64),
    String(RcStr),
    MakeList(u32),
    MakeSet(u32),
    MakeMap(u32),
    NewFunc(Rc<Code>),

    // stack manipulation
    Pop,
    Dup,
    Dup2,
    Swap01,
    Swap12,
    Unpack(u32),

    // variable access
    Get(VarScope, u32),
    Set(VarScope, u32),
    Tee(VarScope, u32),

    // error handling
    AddTry(u32),
    PopTry,
    Throw,

    // control flow
    Goto(u32),
    GotoIfFalse(u32),
    GotoIfTrueElsePop(u32),
    GotoIfFalseElsePop(u32),
    Return,
    Yield,

    // operators
    Next,
    CallFunc(u32),
    Binop(Binop),
    Unop(Unop),
    SetItem,
    Print,
    Disasm,

    // Testing
    AddToTest,
    Assert,
    AssertBinop(AssertBinop),
    AssertThrowFailed,

    // (should come last) unresolved control flow ops
    Label(RcStr),
    UnresolvedAddTry(RcStr),
    UnresolvedGoto(RcStr),
    UnresolvedGotoIfFalse(RcStr),
    UnresolvedGotoIfTrueElsePop(RcStr),
    UnresolvedGotoIfFalseElsePop(RcStr),
}

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Arithmetic(ArithmeticBinop),

    // comparison
    Is,
    IsNot,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    In,
    NotIn,

    // for containers
    Add,
    GetItem,
    Remove,
}

impl Binop {
    pub fn to_assert(&self) -> Option<AssertBinop> {
        match self {
            Self::Is => Some(AssertBinop::Is),
            Self::IsNot => Some(AssertBinop::IsNot),
            Self::Equal => Some(AssertBinop::Equal),
            Self::NotEqual => Some(AssertBinop::NotEqual),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ArithmeticBinop {
    Add,
    Subtract,
    Multiply,
    Divide,
    TruncDivide,
    Remainder,
    Exponentiate,

    // trig
    ATan2,
}

#[derive(Debug, Clone, Copy)]
pub enum AssertBinop {
    Is,
    IsNot,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum Unop {
    Arithmetic(ArithmeticUnop),

    Type, // returns the type of a value

    Len,
    Pop,

    Name,
    Str,
    Repr,
    Not, // logical not

    // concatenate strings
    Cat,
}

#[derive(Debug, Clone, Copy)]
pub enum ArithmeticUnop {
    Negative,
    Positive,

    // trig
    Sin,
    Cos,
    Tan,
    ASin,
    ACos,
    ATan,
}

#[derive(Clone)]
pub struct ArgSpec {
    pub req: Vec<RcStr>,        // required parameters
    pub def: Vec<(RcStr, Val)>, // default parameters
    pub var: Option<RcStr>,     // variadic parameter
}

impl ArgSpec {
    pub fn empty() -> Self {
        Self {
            req: vec![],
            def: vec![],
            var: None,
        }
    }
}

pub struct Code {
    generator: bool,
    name: RcStr,
    argspec: ArgSpec,
    vars: Vec<Var>,
    ops: Vec<Opcode>,
    marks: Vec<Mark>,
    label_map: HashMap<RcStr, u32>,
}

impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Code({})", self.name)
    }
}

fn not_found(mark: Mark, name: &RcStr) -> BasicError {
    BasicError {
        marks: vec![mark],
        message: format!("Label {:?} not found", name),
        help: None,
    }
}

impl Code {
    pub fn new(generator: bool, name: RcStr, argspec: ArgSpec, vars: Vec<Var>) -> Self {
        Self {
            generator,
            name,
            argspec,
            vars,
            ops: vec![],
            marks: vec![],
            label_map: HashMap::new(),
        }
    }
    pub fn generator(&self) -> bool {
        self.generator
    }
    pub fn resolve_labels(&mut self) -> Result<(), BasicError> {
        let mut labels = HashMap::new();
        let mut pos = 0;
        for op in self.ops.iter() {
            if let Opcode::Label(name) = op {
                labels.insert(name.clone(), pos as u32);
            } else {
                pos += 1;
            }
        }
        let old_ops = std::mem::replace(&mut self.ops, vec![]);
        let old_marks = std::mem::replace(&mut self.marks, vec![]);
        let mut new_ops = Vec::new();
        let mut new_marks = Vec::new();
        pos = 0;
        for (i, (mut op, mark)) in old_ops.into_iter().zip(old_marks).enumerate() {
            if let Opcode::Label(_) = op {
                continue;
            }
            match &op {
                Opcode::Label(_) => {}
                Opcode::UnresolvedAddTry(label) => {
                    if let Some(pos) = labels.get(label).cloned() {
                        op = Opcode::AddTry(pos);
                    } else {
                        return Err(not_found(self.marks[i].clone(), label));
                    }
                }
                Opcode::UnresolvedGoto(label) => {
                    if let Some(pos) = labels.get(label).cloned() {
                        op = Opcode::Goto(pos);
                    } else {
                        return Err(not_found(self.marks[i].clone(), label));
                    }
                }
                Opcode::UnresolvedGotoIfFalse(label) => {
                    if let Some(pos) = labels.get(label).cloned() {
                        op = Opcode::GotoIfFalse(pos);
                    } else {
                        return Err(not_found(self.marks[i].clone(), label));
                    }
                }
                Opcode::UnresolvedGotoIfFalseElsePop(label) => {
                    if let Some(pos) = labels.get(label).cloned() {
                        op = Opcode::GotoIfFalseElsePop(pos);
                    } else {
                        return Err(not_found(self.marks[i].clone(), &label));
                    }
                }
                Opcode::UnresolvedGotoIfTrueElsePop(label) => {
                    if let Some(pos) = labels.get(label).cloned() {
                        op = Opcode::GotoIfTrueElsePop(pos);
                    } else {
                        return Err(not_found(self.marks[i].clone(), &label));
                    }
                }
                _ => {}
            }
            new_ops.push(op);
            new_marks.push(mark);
            pos += 1;
        }
        self.label_map = labels;
        self.ops = new_ops;
        self.marks = new_marks;
        Ok(())
    }
    pub fn add(&mut self, op: Opcode, mark: Mark) {
        self.ops.push(op);
        self.marks.push(mark);
        assert_eq!(self.ops.len(), self.marks.len());
    }
    pub fn len(&self) -> usize {
        self.ops.len()
    }
    pub fn name(&self) -> &RcStr {
        &self.name
    }
    pub fn argspec(&self) -> &ArgSpec {
        &self.argspec
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
    pub fn format(&self) -> RcStr {
        use std::fmt::Write;
        let mut ret = String::new();
        let out = &mut ret;
        let mut last_lineno = 0;
        writeln!(out, "## Code for {} ##", self.name).unwrap();
        writeln!(out, "#### labels ####").unwrap();
        let mut labels: Vec<(RcStr, u32)> = self.label_map.clone().into_iter().collect();
        labels.sort_by_key(|a| a.1);
        for (label_name, label_index) in labels {
            writeln!(out, "  {} -> {}", label_name, label_index).unwrap();
        }
        writeln!(out, "#### opcodes ####").unwrap();
        for (i, op) in self.ops.iter().enumerate() {
            let lineno = self.marks[i].lineno();
            let ln = if lineno == last_lineno {
                format!("")
            } else {
                format!("{}", lineno)
            };
            last_lineno = lineno;
            writeln!(out, "  {:>4} {:>4}: {:?}", i, ln, op).unwrap();
        }
        ret.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn enum_sizes() {
        // checking that rust will properly fold nested enums
        assert_eq!(size_of::<Binop>(), size_of::<ArithmeticBinop>());
        assert_eq!(size_of::<Binop>(), size_of::<usize>() * 2);
    }
}
