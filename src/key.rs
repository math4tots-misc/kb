use super::RcStr;
use super::Val;
use super::ValType;
use std::cmp;
use std::collections::HashSet;
use std::hash::Hash;
use std::hash::Hasher;
use std::rc::Rc;

/// Hashable values
#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Key {
    Nil,
    Bool(bool),
    FloatBits(u64),
    Type(ValType),
    String(RcStr),
    List(Vec<Key>),
    Set(HSet),
}

impl Key {
    /// tries the convert the val to a key, but if it can't,
    /// it'll return the first unhashable value that was encountered
    pub fn from_val(val: Val) -> Result<Key, Val> {
        match val {
            Val::Nil => Ok(Self::Nil),
            Val::Bool(b) => Ok(Self::Bool(b)),
            Val::Number(x) => Ok(Self::FloatBits(x.to_bits())),
            Val::Type(x) => Ok(Self::Type(x)),
            Val::String(ptr) => Ok(Self::String(ptr)),
            Val::List(vals) => {
                let vals = match Rc::try_unwrap(vals) {
                    Ok(vals) => vals.into_inner(),
                    Err(vals) => vals.borrow().clone(),
                };
                match vals.into_iter().map(Key::from_val).collect() {
                    Ok(keys) => Ok(Self::List(keys)),
                    Err(val) => Err(val),
                }
            }
            Val::Set(set) => {
                let set = match Rc::try_unwrap(set) {
                    Ok(set) => set.into_inner(),
                    Err(set) => set.borrow().clone(),
                };
                Ok(Key::Set(HSet(set)))
            }
            _ => Err(val),
        }
    }
    pub fn to_val(self) -> Val {
        Val::from(self)
    }
}

impl From<Key> for Val {
    fn from(key: Key) -> Self {
        match key {
            Key::Nil => Self::Nil,
            Key::Bool(b) => Self::Bool(b),
            Key::FloatBits(bits) => Self::Number(f64::from_bits(bits)),
            Key::Type(x) => Self::Type(x),
            Key::String(ptr) => Self::String(ptr),
            Key::List(keys) => {
                let vals: Vec<_> = keys.into_iter().map(Val::from).collect();
                vals.into()
            }
            Key::Set(hset) => hset.0.into(),
        }
    }
}

/// HashSet with that is itself hashable and orderable
#[derive(Clone, PartialEq, Eq)]
pub struct HSet(pub HashSet<Key>);

impl HSet {
    fn sorted_keys(&self) -> Vec<Key> {
        let mut keys: Vec<_> = self.0.clone().into_iter().collect();
        keys.sort();
        keys
    }
}

impl Hash for HSet {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.sorted_keys().hash(state);
    }
}

impl cmp::PartialOrd for HSet {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.sorted_keys().partial_cmp(&other.sorted_keys())
    }
}

impl cmp::Ord for HSet {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.sorted_keys().cmp(&other.sorted_keys())
    }
}
