use super::Code;
use std::cell::RefCell;
use std::cmp;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub enum Val {
    /// E.g. to check for usage before assignment
    Invalid,

    Nil,
    Bool(bool),
    Number(f64),

    /// Use of Rc<String> over Rc<str> is by design --
    /// this allows kb to interoperate with rest of mtots
    /// without copying the String all over the place
    String(Rc<String>),

    List(Rc<RefCell<Vec<Val>>>),

    Func(Func),
}

impl Val {
    pub fn number(&self) -> Option<f64> {
        if let Self::Number(x) = self {
            Some(*x)
        } else {
            None
        }
    }
    pub fn expect_number(&self) -> Result<f64, Val> {
        if let Some(x) = self.number() {
            Ok(x)
        } else {
            Err(Val::String("Expected number".to_owned().into()))
        }
    }
    pub fn list(&self) -> Option<&Rc<RefCell<Vec<Val>>>> {
        if let Self::List(x) = self {
            Some(x)
        } else {
            None
        }
    }
    pub fn expect_list(&self) -> Result<&Rc<RefCell<Vec<Val>>>, Val> {
        if let Some(x) = self.list() {
            Ok(x)
        } else {
            Err(Val::String("Expected list".to_owned().into()))
        }
    }
    pub fn func(&self) -> Option<Rc<Code>> {
        if let Self::Func(x) = self {
            Some(x.0.clone())
        } else {
            None
        }
    }
    pub fn expect_func(&self) -> Result<Rc<Code>, Val> {
        if let Some(x) = self.func() {
            Ok(x)
        } else {
            Err(Val::String("Expected func".to_owned().into()))
        }
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Invalid => write!(f, "<invalid>"),
            Val::Nil => write!(f, "nil"),
            Val::Bool(x) => write!(f, "{}", if *x { "true" } else { "false" }),
            Val::Number(x) => write!(f, "{}", x),
            Val::String(x) => {
                write!(f, "\"")?;
                for c in x.chars() {
                    match c {
                        '\\' => write!(f, "\\\\")?,
                        '\"' => write!(f, "\\\"")?,
                        '\'' => write!(f, "\\\'")?,
                        '\n' => write!(f, "\\\n")?,
                        '\r' => write!(f, "\\\r")?,
                        '\t' => write!(f, "\\\t")?,
                        _ => write!(f, "{}", c)?,
                    }
                }
                write!(f, "\"")
            }
            Val::List(xs) => {
                write!(f, "[")?;
                for (i, x) in xs.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", x)?;
                }
                write!(f, "]")
            }
            Val::Func(func) => write!(f, "<func {}>", func.0.name),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::String(x) => write!(f, "{}", x),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Clone)]
pub struct Func(pub Rc<Code>);

impl cmp::PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        Rc::as_ptr(&self.0) == Rc::as_ptr(&other.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn val_size() {
        assert_eq!(std::mem::size_of::<Val>(), 2 * std::mem::size_of::<usize>());
    }
}
