use super::rterr;
use super::Code;
use super::GenObj;
use super::Key;
use super::RcStr;
use std::any::Any;
use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::cmp;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub enum Val {
    /// E.g. to check for usage before assignment
    Invalid,

    Nil,
    Bool(bool),
    Number(f64),
    Type(ValType),

    String(RcStr),

    Bytes(Rc<Bytes>),
    List(Rc<List>),
    Set(Rc<Set>),
    Map(Rc<Map>),

    Func(Func),

    GenObj(GenObjPtr),

    Class(Rc<Class>),
    Object(Rc<Object>),

    /// Handle to an external resource.
    /// Effectively opaque. I would prefer passing around raw integer values
    /// to refer to external handles for kb, but sometimes you realaly want resources to
    /// be cleaned up for you when you run out of references to the resource
    Handle(Rc<Handle>),
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
            Self::Bytes(_) => ValType::Bytes,
            Self::List(_) => ValType::List,
            Self::Set(_) => ValType::Set,
            Self::Map(_) => ValType::Map,
            Self::Func(_) => ValType::Func,
            Self::GenObj(_) => ValType::GenObj,
            Self::Class(_) => ValType::Class,
            Self::Object(_) => ValType::Object,
            Self::Handle(_) => ValType::Handle,
        }
    }
    pub fn truthy(&self) -> bool {
        match self {
            Self::Invalid => panic!("Val::Invalid.truthy()"),
            Self::Nil => false,
            Self::Bool(x) => *x,
            Self::Number(x) => *x != 0.0,
            Self::String(x) => x.len() > 0,
            Self::Bytes(x) => x.borrow().len() > 0,
            Self::List(x) => x.borrow().len() > 0,
            Self::Set(x) => x.borrow().len() > 0,
            Self::Map(x) => x.borrow().len() > 0,
            Self::Type(_)
            | Self::Func(_)
            | Self::GenObj(_)
            | Self::Class(_)
            | Self::Object(_)
            | Self::Handle(_) => true,
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
            Err(rterr("Expected number"))
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
            Err(rterr("Expected type"))
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
            Err(rterr("Expected string"))
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
            Err(rterr("Expected list"))
        }
    }
    pub fn map(&self) -> Option<&Map> {
        if let Self::Map(x) = self {
            Some(x)
        } else {
            None
        }
    }
    pub fn expect_map(&self) -> Result<&Map, Val> {
        if let Some(x) = self.map() {
            Ok(x)
        } else {
            Err(rterr("Expected map"))
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
            Err(rterr("Expected func"))
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
            Err(rterr("Expected generator object"))
        }
    }
    pub fn expect_byte(&self) -> Result<u8, Val> {
        let number = self.expect_number()?;
        if number < 0.0 || number > 255.0 {
            Err(rterr!("Byte value out of bounds ({})", number))
        } else {
            Ok(number as u8)
        }
    }
    pub fn expect_class(&self) -> Result<&Rc<Class>, Val> {
        if let Self::Class(cls) = self {
            Ok(cls)
        } else {
            Err(rterr!("Expected a class"))
        }
    }
    pub fn expect_object(&self) -> Result<&Rc<Object>, Val> {
        if let Self::Object(obj) = self {
            Ok(obj)
        } else {
            Err(rterr!("Expected an object"))
        }
    }
    pub fn is_handle_raw(&self) -> bool {
        if let Self::Handle(_) = self {
            true
        } else {
            false
        }
    }
    pub fn is_handle<T: Any>(&self) -> bool {
        if let Self::Handle(handle) = self {
            handle.has_type::<T>()
        } else {
            false
        }
    }
    pub fn expect_handle_raw(&self) -> Result<&Rc<Handle>, Val> {
        if let Self::Handle(handle) = self {
            Ok(handle)
        } else {
            Err(rterr!("Expected a Handle"))
        }
    }
    pub fn expect_handle<T: Any>(&self) -> Result<Ref<T>, Val> {
        self.expect_handle_raw()?.borrow()
    }
    pub fn expect_handle_mut<T: Any>(&self) -> Result<RefMut<T>, Val> {
        self.expect_handle_raw()?.borrow_mut()
    }
    pub fn into_handle<T: Any>(self) -> Result<TypedHandle<T>, Val> {
        match self {
            Self::Handle(handle) => {
                if handle.has_type::<T>() {
                    Ok(TypedHandle(handle, PhantomData))
                } else {
                    Err(rterr!(
                        "Expected {:?}, but {:?}",
                        handle.type_name(),
                        std::any::type_name::<T>()
                    ))
                }
            }
            _ => Err(rterr!("Expected a Handle")),
        }
    }
    pub fn into_hcow<T: Any>(self) -> Result<HCow<T>, Val> {
        Ok(HCow::Handle(self.into_handle()?))
    }
    pub fn in_(&self, other: &Self) -> Result<bool, Val> {
        match other {
            Self::String(string) => {
                if let Val::String(query) = self {
                    Ok(string.contains(query.as_ref()))
                } else {
                    Err(rterr!(
                        concat!(
                            "'in' requires both members to be a string ",
                            "if the second argument is a string, but got {:?}",
                        ),
                        other
                    ))
                }
            }
            Self::List(vec) => Ok(vec.borrow().contains(self)),
            Self::Set(set) => {
                let key = match Key::from_val(self.clone()) {
                    Ok(key) => key,
                    Err(val) => return Err(rterr!("{:?} is not hashable", val)),
                };
                Ok(set.borrow().contains(&key))
            }
            Self::Map(map) => {
                let key = match Key::from_val(self.clone()) {
                    Ok(key) => key,
                    Err(val) => return Err(rterr!("{:?} is not hashable", val)),
                };
                Ok(map.borrow().contains_key(&key))
            }
            _ => Err(rterr!("{:?} does not support the 'in' operator", other)),
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
            (Self::Class(a), Self::Class(b)) => a == b,
            (Self::Object(a), Self::Object(b)) => a == b,
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
            _ => Err(rterr!("{} and {} are not comparable", self, other)),
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
    fn add_to_bytes(&self, out: &mut Vec<u8>) -> Result<(), Val> {
        match self {
            Val::Number(_) => out.push(self.expect_byte()?),
            Val::String(string) => out.extend(string.as_bytes()),
            Val::Bytes(bytes) => out.extend(bytes.borrow().iter()),
            Val::List(list) => {
                for x in list.borrow().iter() {
                    x.add_to_bytes(out)?;
                }
            }
            val => {
                return Err(rterr!(
                    "Expected number, string, bytes or list but got {:?}",
                    val
                ))
            }
        }
        Ok(())
    }
    pub fn to_bytes(self) -> Result<Vec<u8>, Val> {
        match self {
            Val::Bytes(bytes) => Ok(match Rc::try_unwrap(bytes) {
                Ok(bytes) => bytes.into_inner(),
                Err(bytes) => bytes.borrow().clone(),
            }),
            _ => {
                let mut out = Vec::new();
                self.add_to_bytes(&mut out)?;
                Ok(out)
            }
        }
    }
    pub fn getattr<S>(&self, attr: &S) -> Result<Val, Val>
    where
        RcStr: std::borrow::Borrow<S>,
        S: fmt::Debug + std::hash::Hash + cmp::Eq,
    {
        let opt = match self {
            Self::Class(cls) => cls.attrs.borrow().get(attr).cloned(),
            Self::Object(obj) => obj.fields.borrow().get(attr).cloned(),
            _ => {
                return Err(rterr!(
                    "getattr requires a Class or Object but got {:?}",
                    self.type_()
                ))
            }
        };
        if let Some(val) = opt {
            Ok(val)
        } else {
            Err(rterr!("Attribute {:?} not found", attr))
        }
    }
    pub fn setattr(&self, attr: RcStr, val: Val) -> Result<(), Val> {
        let mut map = match self {
            Self::Class(cls) => cls.attrs.borrow_mut(),
            Self::Object(obj) => obj.fields.borrow_mut(),
            _ => {
                return Err(rterr!(
                    "setattr requires a Class or Object but got {:?}",
                    self.type_()
                ))
            }
        };
        map.insert(attr, val);
        Ok(())
    }
    pub fn get_method(&self, method_name: &RcStr) -> Result<Val, Val> {
        let obj = self.expect_object()?;
        match obj.cls.methods.get(method_name) {
            Some(method) => Ok(method.clone()),
            None => Err(rterr!(
                "Method {:?} not found for {:?}",
                method_name,
                obj.cls.name
            )),
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

impl From<Vec<u8>> for Val {
    fn from(vec: Vec<u8>) -> Self {
        Self::Bytes(
            Bytes {
                vec: RefCell::new(vec),
            }
            .into(),
        )
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

impl From<Rc<Class>> for Val {
    fn from(cls: Rc<Class>) -> Self {
        Self::Class(cls)
    }
}

impl From<&Rc<Class>> for Val {
    fn from(cls: &Rc<Class>) -> Self {
        Self::Class(cls.clone())
    }
}

impl From<Object> for Val {
    fn from(obj: Object) -> Self {
        Self::Object(Rc::new(obj))
    }
}

impl From<Rc<Object>> for Val {
    fn from(obj: Rc<Object>) -> Self {
        Self::Object(obj)
    }
}

impl From<&Rc<Object>> for Val {
    fn from(obj: &Rc<Object>) -> Self {
        Self::Object(obj.clone())
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
            Val::Bytes(xs) => {
                write!(f, "BYTES([")?;
                for (i, x) in xs.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", x)?;
                }
                write!(f, "])")
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
            Val::Class(cls) => write!(f, "<class {}>", cls.name),
            Val::Object(obj) => write!(f, "<object {}/{:?}>", obj.cls.name, Rc::as_ptr(obj)),
            Val::Handle(handle) => {
                write!(f, "<handle {}/{:?}>", handle.type_name, Rc::as_ptr(handle),)
            }
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
    Bytes,
    List,
    Set,
    Map,
    Func,
    GenObj,
    Class,
    Object,
    Handle,
}

pub struct Bytes {
    vec: RefCell<Vec<u8>>,
}

impl Bytes {
    pub fn borrow(&self) -> Ref<Vec<u8>> {
        self.vec.borrow()
    }
    pub fn borrow_mut(&self) -> RefMut<Vec<u8>> {
        self.vec.borrow_mut()
    }
    pub fn into_inner(self) -> Vec<u8> {
        self.vec.into_inner()
    }
}

impl cmp::PartialEq for Bytes {
    fn eq(&self, other: &Self) -> bool {
        self.vec.eq(&other.vec)
    }
}

impl cmp::Eq for Bytes {}

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
    pub fn to_string_keys(&self) -> Result<HashMap<RcStr, Val>, Val> {
        let mut ret = HashMap::new();
        for (key, val) in self.borrow().iter() {
            match key {
                Key::String(string) => {
                    ret.insert(string.clone(), val.clone());
                }
                _ => return Err(rterr!("Expected string keys but got {:?}", key)),
            }
        }
        Ok(ret)
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

pub struct Class {
    name: RcStr,
    methods: HashMap<RcStr, Val>,
    attrs: RefCell<HashMap<RcStr, Val>>,
}

impl Class {
    pub fn new(
        name: RcStr,
        methods: HashMap<RcStr, Val>,
        attrs: RefCell<HashMap<RcStr, Val>>,
    ) -> Rc<Self> {
        Rc::new(Self {
            name,
            methods,
            attrs,
        })
    }
    pub fn name(&self) -> &RcStr {
        &self.name
    }
    pub fn methods(&self) -> &HashMap<RcStr, Val> {
        &self.methods
    }
    pub fn attrs(&self) -> Ref<HashMap<RcStr, Val>> {
        self.attrs.borrow()
    }
    pub fn attrs_mut(&self) -> RefMut<HashMap<RcStr, Val>> {
        self.attrs.borrow_mut()
    }
}

impl cmp::PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl cmp::Eq for Class {}

pub struct Object {
    cls: Rc<Class>,
    fields: RefCell<HashMap<RcStr, Val>>,
}

impl Object {
    pub fn new(cls: Rc<Class>) -> Self {
        Self {
            cls,
            fields: RefCell::new(HashMap::new()),
        }
    }
    pub fn cls(&self) -> &Rc<Class> {
        &self.cls
    }
    pub fn fields(&self) -> Ref<HashMap<RcStr, Val>> {
        self.fields.borrow()
    }
    pub fn fields_mut(&self) -> RefMut<HashMap<RcStr, Val>> {
        self.fields.borrow_mut()
    }
}

impl cmp::PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl cmp::Eq for Object {}

pub struct Handle {
    type_name: &'static str,
    value: RefCell<Box<dyn Any>>,
}

impl Handle {
    pub fn new<T: Any>(t: T) -> Val {
        Val::Handle(Rc::new(Handle {
            type_name: std::any::type_name::<T>(),
            value: RefCell::new(Box::new(t)),
        }))
    }
    pub fn type_name(&self) -> &'static str {
        self.type_name
    }
    pub fn has_type<T: Any>(&self) -> bool {
        self.value.borrow().as_ref().is::<T>()
    }
    pub fn borrow<T: Any>(&self) -> Result<Ref<T>, Val> {
        if self.has_type::<T>() {
            Ok(self.borrow_unchecked())
        } else {
            Err(rterr!(
                "Handle does not contain {}",
                std::any::type_name::<T>()
            ))
        }
    }
    /// like borrow, but will panic on error
    fn borrow_unchecked<T: Any>(&self) -> Ref<T> {
        Ref::map(self.value.borrow(), |bx| {
            bx.as_ref().downcast_ref::<T>().unwrap()
        })
    }
    pub fn borrow_mut<T: Any>(&self) -> Result<RefMut<T>, Val> {
        if self.has_type::<T>() {
            Ok(self.borrow_mut_unchecked())
        } else {
            Err(rterr!(
                "Handle does not contain {}",
                std::any::type_name::<T>()
            ))
        }
    }
    /// like borrow_mut, but will panic on error
    fn borrow_mut_unchecked<T: Any>(&self) -> RefMut<T> {
        RefMut::map(self.value.borrow_mut(), |bx| {
            bx.as_mut().downcast_mut::<T>().unwrap()
        })
    }
}

impl cmp::PartialEq for Handle {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl cmp::Eq for Handle {}

/// Like Rc<Handle>, but type has already been checked
pub struct TypedHandle<T: Any>(Rc<Handle>, PhantomData<T>);

impl<T: Any> TypedHandle<T> {
    pub fn type_name(&self) -> &'static str {
        self.0.type_name()
    }
    pub fn borrow(&self) -> Ref<T> {
        self.0.borrow_unchecked()
    }
    pub fn borrow_mut(&self) -> RefMut<T> {
        self.0.borrow_mut_unchecked()
    }
    pub fn get(self) -> Result<T, Val> {
        match Rc::try_unwrap(self.0) {
            Ok(handle) => Ok(*handle.value.into_inner().downcast().unwrap()),
            Err(_) => Err(rterr!("Could not move handle (other references exist)")),
        }
    }
}

/// Like Rust's Cow, but instead of reference, the value may be held in a Handle
pub enum HCow<T: Any> {
    Handle(TypedHandle<T>),
    Owned(T),
}

impl<T: Any> HCow<T> {
    pub fn new(t: T) -> Self {
        Self::Owned(t)
    }
    pub fn map_ref<F: FnOnce(&T) -> R, R>(&self, f: F) -> R {
        match self {
            Self::Handle(handle) => f(&handle.borrow()),
            Self::Owned(t) => f(&t),
        }
    }
    pub fn map_mut<F: FnOnce(&mut T) -> R, R>(&mut self, f: F) -> R {
        match self {
            Self::Handle(handle) => f(&mut handle.borrow_mut()),
            Self::Owned(ref mut t) => f(t),
        }
    }
    /// Tries to get the underlying value of this HCow.
    /// if owned, will return the owned value,
    /// if under handle, will try to move the handled value,
    ///     failing if there are other references preventing
    ///     this value from being moved
    pub fn get(self) -> Result<T, Val> {
        match self {
            Self::Handle(handle) => handle.get(),
            Self::Owned(t) => Ok(t),
        }
    }
}

impl<T: Any + Clone> HCow<T> {
    /// Like 'get', but will clone if needed
    pub fn get_or_clone(self) -> T {
        match self {
            Self::Handle(handle) => {
                match Rc::try_unwrap(handle.0) {
                    Ok(handle) => *handle.value.into_inner().downcast().unwrap(),
                    Err(handle) => handle.borrow_unchecked::<T>().clone(),
                }
            }
            Self::Owned(t) => t,
        }
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
