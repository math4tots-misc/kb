use super::Binop;
use super::Code;
use super::Func;
use super::Handler;
use super::Opcode;
use super::Unop;
use super::Val;
use super::Var;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Vm<H: Handler> {
    scope: Scope,
    handler: H,
}

impl<H: Handler> Vm<H> {
    pub fn new(handler: H) -> Self {
        Self {
            scope: Scope::new(),
            handler,
        }
    }
    pub fn exec(&mut self, code: &Code) -> Result<(), Val> {
        for var in code.vars() {
            self.scope.globals.insert(var.name.clone(), Val::Nil);
        }
        exec(&mut self.scope, &mut self.handler, code)?;
        Ok(())
    }
}

pub fn callfunc<H: Handler>(
    scope: &mut Scope,
    handler: &mut H,
    func: &Code,
    args: Vec<Val>,
) -> Result<Val, Val> {
    if args.len() != func.nparams() {
        return Err(Val::String(
            format!("Expected {} args but got {}", func.nparams(), args.len()).into(),
        ));
    }
    scope.push(func.vars());
    for (i, arg) in args.into_iter().enumerate() {
        scope.set(VarScope::Local, i as u32, arg);
    }
    let ret = exec(scope, handler, func)?;
    scope.pop();
    Ok(ret)
}

pub fn exec<H: Handler>(scope: &mut Scope, handler: &mut H, code: &Code) -> Result<Val, Val> {
    let mut i = 0;
    let mut stack = Vec::new();
    while i < code.len() {
        if let Some(ret) = step(scope, handler, code, &mut i, &mut stack)? {
            return Ok(ret);
        }
    }
    assert!(stack.is_empty());
    Ok(Val::Nil)
}

pub fn step<H: Handler>(
    scope: &mut Scope,
    handler: &mut H,
    code: &Code,
    i: &mut usize,
    stack: &mut Vec<Val>,
) -> Result<Option<Val>, Val> {
    let op = code.fetch(*i);
    *i += 1;
    match op {
        Opcode::Nil => {
            stack.push(Val::Nil);
        }
        Opcode::Bool(x) => {
            stack.push(Val::Bool(*x));
        }
        Opcode::Number(x) => {
            stack.push(Val::Number(*x));
        }
        Opcode::String(x) => {
            stack.push(Val::String(x.clone()));
        }
        Opcode::NewList => {
            stack.push(Val::List(Rc::new(RefCell::new(vec![]))));
        }
        Opcode::NewFunc(code) => {
            stack.push(Val::Func(Func(code.clone())));
        }
        Opcode::Pop => {
            stack.pop().unwrap();
        }
        Opcode::Get(vscope, index) => {
            let val = scope.get(*vscope, *index).clone();
            if let Val::Invalid = val {
                return Err(Val::String(
                    format!(
                        "Variable {} used before being set",
                        scope.get_name(*vscope, *index)
                    )
                    .into(),
                ));
            }
            stack.push(val);
        }
        Opcode::Set(vscope, index) => {
            let val = stack.pop().unwrap();
            scope.set(*vscope, *index, val);
        }
        Opcode::Tee(vscope, index) => {
            let val = stack.last().unwrap().clone();
            scope.set(*vscope, *index, val);
        }
        Opcode::Return => {
            let val = stack.pop().unwrap();
            return Ok(Some(val));
        }
        Opcode::CallFunc(argc) => {
            let old_len = stack.len();
            let new_len = old_len - (*argc as usize);
            let args: Vec<Val> = stack.drain(new_len..).collect();
            let func = stack.pop().unwrap().expect_func()?;
            let ret = callfunc(scope, handler, &func, args)?;
            stack.push(ret);
        }
        Opcode::Print => {
            let x = stack.pop().unwrap();
            handler.print(scope, x)?;
        }
        Opcode::Binop(op) => {
            let rhs = stack.pop().unwrap();
            let lhs = stack.pop().unwrap();
            let ret = match op {
                Binop::Add => Val::Number(lhs.expect_number()? + rhs.expect_number()?),
                Binop::Append => {
                    let list = lhs.expect_list()?;
                    list.borrow_mut().push(rhs);
                    lhs
                }
                _ => panic!("TODO: Binop {:?}", op),
            };
            stack.push(ret);
        }
        Opcode::Unop(op) => {
            let val = stack.pop().unwrap();
            let ret = match op {
                Unop::Negative => Val::Number(-val.expect_number()?),
                Unop::Positive => Val::Number(val.expect_number()?),
            };
            stack.push(ret);
        }
        Opcode::Label(_) => {}
        Opcode::Goto(id) => {
            *i = code.label_map()[*id as usize];
        }
        Opcode::GotoIf(id) => {
            let item = stack.pop().unwrap();
            if item.truthy() {
                *i = code.label_map()[*id as usize];
            }
        }
        Opcode::GotoIfNoPop(id) => {
            let item = stack.last().unwrap();
            if item.truthy() {
                *i = code.label_map()[*id as usize];
            }
        }
    }
    Ok(None)
}

#[derive(Debug, Clone, Copy)]
pub enum VarScope {
    Local,
    Global,
}

pub struct Scope {
    globals: IndexedMap,
    locals: Vec<IndexedMap>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            globals: IndexedMap::new(),
            locals: vec![],
        }
    }
    pub fn get_name(&self, vscope: VarScope, index: u32) -> &Rc<String> {
        match vscope {
            VarScope::Global => self.globals.get_key(index).unwrap(),
            VarScope::Local => self.locals.last().unwrap().get_key(index).unwrap(),
        }
    }
    pub fn get(&self, vscope: VarScope, index: u32) -> &Val {
        match vscope {
            VarScope::Global => &self.globals.values[index as usize],
            VarScope::Local => &self.locals.last().unwrap().values[index as usize],
        }
    }
    pub fn set(&mut self, vscope: VarScope, index: u32, val: Val) {
        match vscope {
            VarScope::Global => self.globals.values[index as usize] = val,
            VarScope::Local => self.locals.last_mut().unwrap().values[index as usize] = val,
        }
    }
    pub fn push(&mut self, locals: &Vec<Var>) {
        let mut map = IndexedMap::new();
        for var in locals {
            let index = map.insert(var.name.clone(), Val::Nil);
            assert_eq!(index, var.index);
        }
        self.locals.push(map);
    }
    pub fn pop(&mut self) {
        self.locals.pop().unwrap();
    }
}

pub struct IndexedMap {
    values: Vec<Val>,
    map: HashMap<Rc<String>, u32>,
}

impl IndexedMap {
    pub fn new() -> Self {
        Self {
            values: vec![],
            map: HashMap::new(),
        }
    }
    pub fn insert(&mut self, key: Rc<String>, val: Val) -> u32 {
        let i = self.values.len() as u32;
        self.values.push(val);
        self.map.insert(key, i);
        i
    }
    pub fn get_by_key(&self, key: &Rc<String>) -> Option<&Val> {
        self.map.get(key).map(|i| &self.values[*i as usize])
    }
    pub fn get_by_index(&self, i: u32) -> Option<&Val> {
        self.values.get(i as usize)
    }
    pub fn get_key(&self, index: u32) -> Option<&Rc<String>> {
        for (key, i) in &self.map {
            if *i == index {
                return Some(key);
            }
        }
        None
    }
}
