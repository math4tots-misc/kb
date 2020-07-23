use super::ArgSpec;
use super::ArithmeticBinop;
use super::ArithmeticUnop;
use super::Binop;
use super::Code;
use super::Func;
use super::GenObjPtr;
use super::Handler;
use super::Mark;
use super::Opcode;
use super::RcStr;
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
    pub fn trace(&self) -> &Vec<Mark> {
        &self.scope.trace
    }
}

pub fn callfunc<H: Handler>(
    scope: &mut Scope,
    handler: &mut H,
    func: &Code,
    mut args: Vec<Val>,
) -> Result<Val, Val> {
    // match args against parameters
    let spec = func.argspec();
    prepare_args(spec, &mut args)?;

    // initialize args: save the values into local vars
    scope.push(func.vars());
    for (i, arg) in args.into_iter().enumerate() {
        scope.set(VarScope::Local, i as u32, arg);
    }
    let ret = exec(scope, handler, func)?;
    scope.pop();
    Ok(ret)
}

fn mkgenobj(func: Rc<Code>, mut args: Vec<Val>) -> Result<Val, Val> {
    prepare_args(func.argspec(), &mut args)?;
    let locals = new_locals_from_vars(func.vars());
    let genobj = GenObj {
        code: func,
        locals,
        i: 0,
        stack: vec![],
    };
    Ok(Val::GenObj(GenObjPtr(Rc::new(RefCell::new(genobj)))))
}

fn prepare_args(spec: &ArgSpec, args: &mut Vec<Val>) -> Result<(), Val> {
    if spec.var.is_some() || spec.def.len() > 0 {
        // has variadic parameter or at least one default parameter
        let argc = args.len();
        let argmin = spec.req.len();
        let argmax = argmin + spec.def.len();
        if spec.var.is_some() {
            if argc < argmin {
                return Err(Val::String(
                    format!("Expected at least {} args but got {}", argmin, argc).into(),
                ));
            }
        } else {
            if argc < argmin || argc > argmax {
                return Err(Val::String(
                    format!("Expected {} to {} args but got {}", argmin, argmax, argc).into(),
                ));
            }
        }
        let def = &spec.def;
        for i in argc..argmax {
            let default_val = def[i - argmin].1.clone();
            args.push(default_val);
        }
        if spec.var.is_some() {
            let rem_args: Vec<Val> = if argmax < argc {
                args.drain(argmax..argc).collect()
            } else {
                vec![]
            };
            args.push(rem_args.into());
        }
    } else {
        // no variadic parameter and no default parameters
        if args.len() != spec.req.len() {
            return Err(Val::String(
                format!("Expected {} args but got {}", spec.req.len(), args.len()).into(),
            ));
        }
    }
    Ok(())
}

pub fn exec<H: Handler>(scope: &mut Scope, handler: &mut H, code: &Code) -> Result<Val, Val> {
    let mut i = 0;
    let mut stack = Vec::new();
    while i < code.len() {
        match step(scope, handler, code, &mut i, &mut stack)? {
            StepVal::Return(val) => return Ok(val),
            StepVal::Yield(_) => return Err("Yielding does not make sense here".into()),
            StepVal::None => {}
        }
    }
    assert!(stack.is_empty());
    Ok(Val::Nil)
}

enum StepVal {
    Return(Val),
    Yield(Val),
    None,
}

fn step<H: Handler>(
    scope: &mut Scope,
    handler: &mut H,
    code: &Code,
    i: &mut usize,
    stack: &mut Vec<Val>,
) -> Result<StepVal, Val> {
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
        Opcode::MakeList(len) => {
            let start = stack.len() - *len as usize;
            let list: Vec<_> = stack.drain(start..).collect();
            stack.push(list.into());
        }
        Opcode::NewFunc(code) => {
            stack.push(Val::Func(Func(code.clone())));
        }
        Opcode::Pop => {
            stack.pop().unwrap();
        }
        Opcode::Dup => {
            let x = stack.last().unwrap().clone();
            stack.push(x);
        }
        Opcode::Unpack(n) => {
            let elements = stack.pop().unwrap();
            if let Some(list) = elements.list() {
                if list.borrow().len() != *n as usize {
                    scope.push_trace(code.marks()[*i - 1].clone());
                    return Err(Val::String(
                        format!(
                            "Expected {} values, but got a list with {} values",
                            n,
                            list.borrow().len(),
                        )
                        .into(),
                    ));
                }
                for item in list.borrow().iter() {
                    stack.push(item.clone());
                }
            } else {
                let genobj = elements.expect_genobj()?;
                let mut genobj = genobj.0.borrow_mut();
                let vec = genobj.to_vec(scope, handler)?;
                if vec.len() != *n as usize {
                    scope.push_trace(code.marks()[*i - 1].clone());
                    return Err(Val::String(
                        format!("Expected {} values, but got {} values", n, vec.len(),).into(),
                    ));
                }
                stack.extend(vec);
            }
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
            return Ok(StepVal::Return(val));
        }
        Opcode::Yield => {
            let val = stack.pop().unwrap();
            return Ok(StepVal::Yield(val));
        }
        Opcode::Next => {
            let genobj = stack.pop().unwrap();
            let genobj = genobj.expect_genobj()?;
            let mut genobj = genobj.0.borrow_mut();
            match genobj.resume(scope, handler, Val::Nil)? {
                Some(val) => {
                    stack.push(val);
                    stack.push(true.into());
                }
                None => {
                    stack.push(Val::Nil);
                    stack.push(false.into());
                }
            }
        }
        Opcode::CallFunc(argc) => {
            let old_len = stack.len();
            let new_len = old_len - (*argc as usize);
            let args: Vec<Val> = stack.drain(new_len..).collect();
            let func = stack.pop().unwrap();
            let func = if let Some(func) = func.func() {
                func
            } else {
                scope.push_trace(code.marks()[*i - 1].clone());
                return Err(format!("{} is not a function", func).into())
            };

            scope.push_trace(code.marks()[*i - 1].clone());
            if func.generator() {
                // generator; create a generator object
                let genobj = mkgenobj(func.clone(), args)?;
                stack.push(genobj);
            } else {
                // this is a normal function, call it
                let ret = callfunc(scope, handler, &func, args)?;
                stack.push(ret);
            }
            scope.pop_trace();
        }
        Opcode::Print => {
            let x = stack.pop().unwrap();
            handler.print(scope, x)?;
        }
        Opcode::Binop(op) => {
            let rhs = stack.pop().unwrap();
            let lhs = stack.pop().unwrap();
            let ret = match op {
                // arithmetic operators
                Binop::Arithmetic(aop) => {
                    let lhs = if let Some(lhs) = lhs.number() {
                        lhs
                    } else {
                        scope.push_trace(code.marks()[*i - 1].clone());
                        return Err(format!(
                            concat!(
                                "The left hand side of this arithmetic operation ",
                                "should be a number but got {:?}"
                            ),
                            lhs
                        )
                        .into());
                    };
                    let rhs = if let Some(rhs) = rhs.number() {
                        rhs
                    } else {
                        scope.push_trace(code.marks()[*i - 1].clone());
                        return Err(format!(
                            concat!(
                                "The right hand side of this arithmetic operation ",
                                "should be a number but got {:?}"
                            ),
                            rhs
                        )
                        .into());
                    };
                    match aop {
                        ArithmeticBinop::Add => Val::Number(lhs + rhs),
                        ArithmeticBinop::Subtract => Val::Number(lhs - rhs),
                        ArithmeticBinop::Multiply => Val::Number(lhs * rhs),
                        ArithmeticBinop::Divide => Val::Number(lhs / rhs),
                        ArithmeticBinop::TruncDivide => Val::Number((lhs / rhs).trunc()),
                        ArithmeticBinop::Remainder => Val::Number(lhs % rhs),
                    }
                }

                // comparison operators
                Binop::LessThan => Val::Bool(lhs.lt(&rhs)?),
                Binop::LessThanOrEqual => Val::Bool(!rhs.lt(&lhs)?),
                Binop::GreaterThan => Val::Bool(rhs.lt(&lhs)?),
                Binop::GreaterThanOrEqual => Val::Bool(!lhs.lt(&rhs)?),

                // list
                Binop::Append => {
                    let list = lhs.expect_list()?;
                    list.borrow_mut().push(rhs);
                    lhs
                }
            };
            stack.push(ret);
        }
        Opcode::Unop(op) => {
            let val = stack.pop().unwrap();
            let ret = match op {
                Unop::Arithmetic(aop) => {
                    let val = if let Some(val) = val.number() {
                        val
                    } else {
                        scope.push_trace(code.marks()[*i - 1].clone());
                        return Err(format!(
                            concat!(
                                "The argument to this unary arithmetic operator ",
                                "should be a number but got {:?}"
                            ),
                            val
                        )
                        .into());
                    };
                    match aop {
                        ArithmeticUnop::Negative => Val::Number(-val),
                        ArithmeticUnop::Positive => Val::Number(val),
                    }
                }
            };
            stack.push(ret);
        }
        Opcode::Goto(pos) => {
            *i = *pos as usize;
        }
        Opcode::GotoIfFalse(pos) => {
            let item = stack.pop().unwrap();
            if !item.truthy() {
                *i = *pos as usize;
            }
        }
        Opcode::GotoIfFalseNoPop(pos) => {
            let item = stack.last().unwrap();
            if !item.truthy() {
                *i = *pos as usize;
            }
        }
        Opcode::Label(_)
        | Opcode::UnresolvedGoto(_)
        | Opcode::UnresolvedGotoIfFalse(_)
        | Opcode::UnresolvedGotoIfFalseNoPop(_) => {
            panic!("Unresolved opcode: {:?}", op);
        }
    }
    Ok(StepVal::None)
}

#[derive(Debug, Clone, Copy)]
pub enum VarScope {
    Local,
    Global,
}

pub struct Scope {
    globals: IndexedMap,
    locals: Vec<IndexedMap>,
    trace: Vec<Mark>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            globals: IndexedMap::new(),
            locals: vec![],
            trace: vec![],
        }
    }
    pub fn get_name(&self, vscope: VarScope, index: u32) -> &RcStr {
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
        self.locals.push(new_locals_from_vars(locals));
    }
    pub fn push_existing_locals(&mut self, map: IndexedMap) {
        self.locals.push(map);
    }
    pub fn pop(&mut self) {
        self.locals.pop().unwrap();
    }
    pub fn push_trace(&mut self, mark: Mark) {
        self.trace.push(mark);
    }
    pub fn pop_trace(&mut self) {
        self.trace.pop();
    }
}

fn new_locals_from_vars(vars: &Vec<Var>) -> IndexedMap {
    let mut map = IndexedMap::new();
    for var in vars {
        let index = map.insert(var.name.clone(), Val::Nil);
        assert_eq!(index, var.index);
    }
    map
}

pub struct IndexedMap {
    values: Vec<Val>,
    map: HashMap<RcStr, u32>,
}

impl IndexedMap {
    pub fn new() -> Self {
        Self {
            values: vec![],
            map: HashMap::new(),
        }
    }
    pub fn insert(&mut self, key: RcStr, val: Val) -> u32 {
        let i = self.values.len() as u32;
        self.values.push(val);
        self.map.insert(key, i);
        i
    }
    pub fn get_by_key(&self, key: &RcStr) -> Option<&Val> {
        self.map.get(key).map(|i| &self.values[*i as usize])
    }
    pub fn get_by_index(&self, i: u32) -> Option<&Val> {
        self.values.get(i as usize)
    }
    pub fn get_key(&self, index: u32) -> Option<&RcStr> {
        for (key, i) in &self.map {
            if *i == index {
                return Some(key);
            }
        }
        None
    }
    pub fn len(&self) -> usize {
        self.values.len()
    }
}

/// generator object
/// created by calling generator functions
pub struct GenObj {
    code: Rc<Code>,
    locals: IndexedMap,
    i: usize,
    stack: Vec<Val>,
}

impl GenObj {
    pub fn resume<H: Handler>(
        &mut self,
        scope: &mut Scope,
        handler: &mut H,
        val: Val,
    ) -> Result<Option<Val>, Val> {
        if self.i >= self.code.len() {
            return Ok(None);
        }
        scope.push_existing_locals(std::mem::replace(&mut self.locals, IndexedMap::new()));
        self.stack.push(val);
        let result = self.loop_(scope, handler);
        scope.pop();
        result
    }
    pub fn to_vec<H: Handler>(
        &mut self,
        scope: &mut Scope,
        handler: &mut H,
    ) -> Result<Vec<Val>, Val> {
        let mut ret = Vec::new();
        while let Some(val) = self.resume(scope, handler, Val::Nil)? {
            ret.push(val);
        }
        Ok(ret)
    }
    fn loop_<H: Handler>(
        &mut self,
        scope: &mut Scope,
        handler: &mut H,
    ) -> Result<Option<Val>, Val> {
        while self.i < self.code.len() {
            match step(scope, handler, &self.code, &mut self.i, &mut self.stack)? {
                StepVal::Return(_) => {
                    self.i = self.code.len();
                    self.clear(); // release all local vars etc
                    return Ok(None);
                }
                StepVal::Yield(val) => return Ok(Some(val)),
                StepVal::None => {}
            }
        }
        self.clear();
        Ok(None)
    }
    fn clear(&mut self) {
        self.stack = vec![];
        self.locals = IndexedMap::new();
    }
}
