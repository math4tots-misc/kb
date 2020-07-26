use super::Code;
use super::GenObj;
use super::Key;
use super::RcStr;
use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::cmp;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub enum Val {
    /// E.g. to check for usage before assignment
    Invalid,

    Nil,
    Bool(bool),
    Number(f64),
    Type(ValType),

    /// Use of RcStr over Rc<str> is by design --
    /// this allows kb to interoperate with rest of mtots
    /// without copying the String all over the place
    String(RcStr),

    List(Rc<List>),
    Set(Rc<Set>),
    Map(Rc<Map>),

    Func(Func),

    GenObj(GenObjPtr),
}

impl Val {
    pub fn type_(&self) -> ValType {
        match self {
            Self::Invalid => panic!("Val::Invalid.type_()"),
            Self::Nil => ValType::Nil,
            Self::Bool(_) => ValType::Bool,
            Self::Number(_) => ValType::Number,
            Self::Type(_) => ValType::Type,
            Self::String(_) => ValType::String,
            Self::List(_) => ValType::List,
            Self::Set(_) => ValType::Set,
            Self::Map(_) => ValType::Map,
            Self::Func(_) => ValType::Func,
            Self::GenObj(_) => ValType::GenObj,
        }
    }
    pub fn truthy(&self) -> bool {
        match self {
            Self::Invalid => panic!("Val::Invalid.truthy()"),
            Self::Nil => false,
            Self::Bool(x) => *x,
            Self::Number(x) => *x != 0.0,
            Self::String(x) => x.len() > 0,
            Self::List(x) => x.borrow().len() > 0,
            Self::Set(x) => x.borrow().len() > 0,
            Self::Map(x) => x.borrow().len() > 0,
            Self::Type(_) | Self::Func(_) | Self::GenObj(_) => true,
        }
    }
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
    pub fn as_type(&self) -> Option<ValType> {
        if let Self::Type(x) = self {
            Some(*x)
        } else {
            None
        }
    }
    pub fn expect_type(&self) -> Result<ValType, Val> {
        if let Some(x) = self.as_type() {
            Ok(x)
        } else {
            Err(Val::String("Expected type".to_owned().into()))
        }
    }
    pub fn string(&self) -> Option<&RcStr> {
        if let Self::String(x) = self {
            Some(x)
        } else {
            None
        }
    }
    pub fn expect_string(&self) -> Result<&RcStr, Val> {
        if let Some(x) = self.string() {
            Ok(x)
        } else {
            Err(Val::String("Expected string".to_owned().into()))
        }
    }
    pub fn list(&self) -> Option<&List> {
        if let Self::List(x) = self {
            Some(x)
        } else {
            None
        }
    }
    pub fn expect_list(&self) -> Result<&List, Val> {
        if let Some(x) = self.list() {
            Ok(x)
        } else {
            Err(Val::String("Expected list".to_owned().into()))
        }
    }
    pub fn func(&self) -> Option<&Rc<Code>> {
        if let Self::Func(x) = self {
            Some(&x.0)
        } else {
            None
        }
    }
    pub fn expect_func(&self) -> Result<&Rc<Code>, Val> {
        if let Some(x) = self.func() {
            Ok(x)
        } else {
            Err(Val::String("Expected func".to_owned().into()))
        }
    }
    pub fn genobj(&self) -> Option<&GenObjPtr> {
        if let Self::GenObj(x) = self {
            Some(x)
        } else {
            None
        }
    }
    pub fn expect_genobj(&self) -> Result<&GenObjPtr, Val> {
        if let Some(x) = self.genobj() {
            Ok(x)
        } else {
            Err(Val::String("Expected generator object".to_owned().into()))
        }
    }
    pub fn in_(&self, other: &Self) -> Result<bool, Val> {
        match other {
            Self::String(string) => {
                if let Val::String(query) = self {
                    Ok(string.contains(query.as_ref()))
                } else {
                    Err(Val::String(
                        format!(
                            concat!(
                                "'in' requires both members to be a string ",
                                "if the second argument is a string, but got {:?}",
                            ),
                            other
                        )
                        .into(),
                    ))
                }
            }
            Self::List(vec) => Ok(vec.borrow().contains(self)),
            Self::Set(set) => {
                let key = match Key::from_val(self.clone()) {
                    Ok(key) => key,
                    Err(val) => return Err(format!("{:?} is not hashable", val).into()),
                };
                Ok(set.borrow().contains(&key))
            }
            Self::Map(map) => {
                let key = match Key::from_val(self.clone()) {
                    Ok(key) => key,
                    Err(val) => return Err(format!("{:?} is not hashable", val).into()),
                };
                Ok(map.borrow().contains_key(&key))
            }
            _ => Err(Val::String(
                format!("{:?} does not support the 'in' operator", other).into(),
            )),
        }
    }
    pub fn is(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::Type(a), Self::Type(b)) => a == b,
            (Self::String(a), Self::String(b)) => a.as_ptr() == b.as_ptr(),
            (Self::List(a), Self::List(b)) => Rc::as_ptr(a) == Rc::as_ptr(b),
            (Self::Set(a), Self::Set(b)) => Rc::as_ptr(a) == Rc::as_ptr(b),
            (Self::Map(a), Self::Map(b)) => Rc::as_ptr(a) == Rc::as_ptr(b),
            (Self::Func(a), Self::Func(b)) => a == b,
            (Self::GenObj(a), Self::GenObj(b)) => a == b,
            _ => false,
        }
    }
    pub fn cmp(&self, other: &Self) -> Result<cmp::Ordering, Val> {
        match (self, other) {
            (Self::Number(a), Self::Number(b)) => {
                Ok(f64::partial_cmp(a, b).unwrap_or(cmp::Ordering::Equal))
            }
            (Self::String(a), Self::String(b)) => Ok(a.cmp(&b)),
            (Self::List(a), Self::List(b)) => Ok({
                let a = a.borrow();
                let b = b.borrow();
                for (x, y) in a.iter().zip(b.iter()) {
                    match x.cmp(y)? {
                        cmp::Ordering::Equal => {}
                        ord => return Ok(ord),
                    }
                }
                a.len().cmp(&b.len())
            }),
            (Self::Set(a), Self::Set(b)) => Ok(a.sorted_keys().cmp(&b.sorted_keys())),
            _ => Err(Val::String(
                format!("{} and {} are not comparable", self, other).into(),
            )),
        }
    }
    pub fn lt(&self, other: &Self) -> Result<bool, Val> {
        Ok(self.cmp(other)? == cmp::Ordering::Less)
    }
    pub fn as_err(&self) -> ErrVal {
        ErrVal(self)
    }
    pub fn try_key_val_pair(&self) -> Option<(Key, Val)> {
        match self {
            Val::List(list) if list.borrow().len() == 2 => {
                let key = match Key::from_val(list.borrow()[0].clone()) {
                    Ok(key) => key,
                    Err(_) => return None,
                };
                let val = list.borrow()[1].clone();
                Some((key, val))
            }
            _ => None,
        }
    }
}

impl From<bool> for Val {
    fn from(x: bool) -> Self {
        Self::Bool(x)
    }
}

impl From<f64> for Val {
    fn from(x: f64) -> Self {
        Self::Number(x)
    }
}

impl From<&str> for Val {
    fn from(s: &str) -> Self {
        Self::String(s.into())
    }
}

impl From<String> for Val {
    fn from(s: String) -> Self {
        Self::String(s.into())
    }
}

impl From<&RcStr> for Val {
    fn from(s: &RcStr) -> Self {
        Self::String(s.clone())
    }
}

impl From<RcStr> for Val {
    fn from(s: RcStr) -> Self {
        Self::String(s.into())
    }
}

impl From<Vec<Val>> for Val {
    fn from(vec: Vec<Val>) -> Self {
        Self::List(
            List {
                vec: RefCell::new(vec),
            }
            .into(),
        )
    }
}

impl From<HashSet<Key>> for Val {
    fn from(set: HashSet<Key>) -> Self {
        Self::Set(
            Set {
                set: RefCell::new(set),
            }
            .into(),
        )
    }
}

impl From<HashMap<Key, Val>> for Val {
    fn from(map: HashMap<Key, Val>) -> Self {
        Self::Map(
            Map {
                map: RefCell::new(map),
            }
            .into(),
        )
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Invalid => write!(f, "<invalid>"),
            Val::Nil => write!(f, "nil"),
            Val::Bool(x) => write!(f, "{}", if *x { "true" } else { "false" }),
            Val::Number(x) => write!(f, "{}", x),
            Val::Type(x) => write!(f, "{:?}", x),
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
            Val::Set(xs) => {
                write!(f, "{{")?;
                for (i, x) in xs.sorted_keys().into_iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", x.clone().to_val())?;
                }
                write!(f, "}}")
            }
            Val::Map(xs) => {
                write!(f, "{{")?;
                if xs.borrow().is_empty() {
                    write!(f, ":")?;
                } else {
                    for (i, (k, v)) in xs.sorted_pairs().into_iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{:?}: {:?}", k.clone().to_val(), v)?;
                    }
                }
                write!(f, "}}")
            }
            Val::Func(func) => write!(f, "<func {}>", func.0.name()),
            Val::GenObj(_) => write!(f, "<genobj>"),
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

pub struct ErrVal<'a>(&'a Val);

impl<'a> fmt::Display for ErrVal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Val::List(list) if list.borrow().len() == 2 => {
                write!(f, "{}: {}", list.borrow()[0], list.borrow()[1])
            }
            _ => write!(f, "ERROR: {}", self.0),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum ValType {
    Nil,
    Bool,
    Number,
    Type,
    String,
    List,
    Set,
    Map,
    Func,
    GenObj,
}

/// Wrapper around RefCell<Vec<Val>>
/// Having a wrapper keeps the possibility open for e.g.
/// caching hash values, or mutability locks
pub struct List {
    vec: RefCell<Vec<Val>>,
}

impl List {
    pub fn borrow(&self) -> Ref<Vec<Val>> {
        self.vec.borrow()
    }
    pub fn borrow_mut(&self) -> RefMut<Vec<Val>> {
        self.vec.borrow_mut()
    }
    pub fn into_inner(self) -> Vec<Val> {
        self.vec.into_inner()
    }
}

impl cmp::PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        self.vec.eq(&other.vec)
    }
}

impl cmp::Eq for List {}

#[derive(PartialEq, Eq)]
pub struct Set {
    set: RefCell<HashSet<Key>>,
}

impl Set {
    pub fn borrow(&self) -> Ref<HashSet<Key>> {
        self.set.borrow()
    }
    pub fn borrow_mut(&self) -> RefMut<HashSet<Key>> {
        self.set.borrow_mut()
    }
    pub fn into_inner(self) -> HashSet<Key> {
        self.set.into_inner()
    }
    pub fn sorted_keys(&self) -> Vec<Key> {
        let mut vec: Vec<_> = self.borrow().clone().into_iter().collect();
        vec.sort();
        vec
    }
}

#[derive(PartialEq)]
pub struct Map {
    map: RefCell<HashMap<Key, Val>>,
}

impl Map {
    pub fn borrow(&self) -> Ref<HashMap<Key, Val>> {
        self.map.borrow()
    }
    pub fn borrow_mut(&self) -> RefMut<HashMap<Key, Val>> {
        self.map.borrow_mut()
    }
    pub fn into_inner(self) -> HashMap<Key, Val> {
        self.map.into_inner()
    }
    pub fn sorted_pairs(&self) -> Vec<(Key, Val)> {
        let mut vec: Vec<_> = self.borrow().clone().into_iter().collect();
        vec.sort_by(|a, b| a.0.cmp(&b.0));
        vec
    }
}

#[derive(Clone)]
pub struct Func(pub Rc<Code>);

impl cmp::PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        Rc::as_ptr(&self.0) == Rc::as_ptr(&other.0)
    }
}

impl cmp::Eq for Func {}

#[derive(Clone)]
pub struct GenObjPtr(pub Rc<RefCell<GenObj>>);

impl cmp::PartialEq for GenObjPtr {
    fn eq(&self, other: &Self) -> bool {
        Rc::as_ptr(&self.0) == Rc::as_ptr(&other.0)
    }
}

impl cmp::Eq for GenObjPtr {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn val_size() {
        assert_eq!(std::mem::size_of::<Val>(), 2 * std::mem::size_of::<usize>());
    }
}
