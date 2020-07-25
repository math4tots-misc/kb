use super::ArgSpec;
use super::ArithmeticBinop;
use super::ArithmeticUnop;
use super::AssertBinop;
use super::Binop;
use super::Code;
use super::Func;
use super::GenObjPtr;
use super::Handler;
use super::Key;
use super::Mark;
use super::Opcode;
use super::RcStr;
use super::Unop;
use super::Val;
use super::Var;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

macro_rules! rterr {
    ( $($args:expr),+ $(,)?) => {
        rterr(format!( $($args),+ ))
    };
}

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
    pub fn run_tests(&mut self, prefixes: &Vec<RcStr>) -> Result<(), Val> {
        let tests = std::mem::replace(&mut self.scope.tests, vec![]);
        for test in tests {
            if prefixes
                .iter()
                .any(|prefix| test.name().starts_with(prefix.as_ref()))
            {
                print!("  test {}... ", test.name());
                callfunc(&mut self.scope, &mut self.handler, &test, vec![])?;
                println!("ok");
            }
        }
        Ok(())
    }
    pub fn exec_and_run_tests(&mut self, code: &Code, prefixes: &Vec<RcStr>) -> Result<(), Val> {
        self.exec(code)?;
        self.run_tests(prefixes)
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
    let mut locals = new_locals_from_vars(func.vars());
    for (i, arg) in args.into_iter().enumerate() {
        locals.set_by_index(i as u32, arg);
    }
    let genobj = GenObj {
        code: func,
        locals,
        i: 0,
        stack: vec![],
        trystack: vec![],
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
                return Err(rterr!("Expected at least {} args but got {}", argmin, argc));
            }
        } else {
            if argc < argmin || argc > argmax {
                return Err(rterr!(
                    "Expected {} to {} args but got {}",
                    argmin,
                    argmax,
                    argc
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
            return Err(rterr!(
                "Expected {} args but got {}",
                spec.req.len(),
                args.len()
            ));
        }
    }
    Ok(())
}

pub fn exec<H: Handler>(scope: &mut Scope, handler: &mut H, code: &Code) -> Result<Val, Val> {
    let mut i = 0;
    let mut stack = Vec::new();
    let mut trystack = Vec::new();
    while i < code.len() {
        match step(scope, handler, code, &mut i, &mut stack, &mut trystack)? {
            StepVal::None => {}
            StepVal::Return(val) => return Ok(val),
            StepVal::Yield(_) => return Err("Yielding does not make sense here".into()),
        }
    }
    assert!(stack.is_empty());
    assert!(trystack.is_empty());
    Ok(Val::Nil)
}

enum StepVal {
    None,
    Return(Val),
    Yield(Val),
}

fn step<H: Handler>(
    scope: &mut Scope,
    handler: &mut H,
    code: &Code,
    i: &mut usize,
    stack: &mut Vec<Val>,
    trystack: &mut Vec<(u32, u32)>,
) -> Result<StepVal, Val> {
    let op = code.fetch(*i);
    *i += 1;

    macro_rules! addtrace {
        () => {
            scope.push_trace(code.marks()[*i - 1].clone());
        };
    }

    macro_rules! handle_error {
        ($err:expr $(,)?) => {{
            let err: Val = $err;
            if let Some((tracelen, pos)) = trystack.pop() {
                scope.trunc_tracelen(tracelen as usize);
                *i = pos as usize;
                stack.clear();
                stack.push(err);
                return Ok(StepVal::None);
            } else {
                return Err(err);
            }
        }};
    }

    // for better error handling by including the stack
    // trace whenever we need to throw
    macro_rules! get0 {
        ($r:expr) => {
            match $r {
                Ok(t) => t,
                Err(err) => {
                    addtrace!();
                    handle_error!(err);
                }
            }
        };
    }

    // like 'get0', but for situations where you want trace information
    // to be added before the expression is evaluated
    macro_rules! get1 {
        ($r:expr) => {{
            addtrace!();
            match $r {
                Ok(t) => {
                    scope.pop_trace();
                    t
                }
                Err(err) => {
                    handle_error!(err);
                }
            }
        }};
    }

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
        Opcode::MakeSet(len) => {
            let start = stack.len() - *len as usize;
            let set: Result<HashSet<_>, Val> = stack.drain(start..).map(Key::from_val).collect();
            match set {
                Ok(set) => {
                    stack.push(set.into());
                }
                Err(val) => {
                    addtrace!();
                    handle_error!(rterr!(
                        concat!(
                            "Encountered an unhashable value ({:?}) while ",
                            "trying to create a set",
                        ),
                        val,
                    ));
                }
            }
        }
        Opcode::MakeMap(len) => {
            let start = stack.len() - 2 * *len as usize;
            let mut map = HashMap::new();
            let vals: Vec<_> = stack.drain(start..).collect();
            let mut vals = vals.into_iter();
            while let Some(key) = vals.next() {
                let key = match Key::from_val(key) {
                    Ok(key) => key,
                    Err(key) => {
                        addtrace!();
                        handle_error!(rterr!(
                            concat!(
                                "Encountered an unhashable key ({:?}) while ",
                                "trying to create a map",
                            ),
                            key,
                        ));
                    }
                };
                let val = vals.next().unwrap();
                map.insert(key, val);
            }
            stack.push(map.into());
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
        Opcode::Dup2 => {
            let len = stack.len();
            let a = stack[len - 2].clone();
            let b = stack[len - 1].clone();
            stack.push(a);
            stack.push(b);
        }
        Opcode::Swap01 => {
            let len = stack.len();
            stack.swap(len - 2, len - 1);
        }
        Opcode::Swap12 => {
            let len = stack.len();
            stack.swap(len - 3, len - 2);
        }
        Opcode::Unpack(n) => {
            let elements = stack.pop().unwrap();
            if let Some(list) = elements.list() {
                if list.borrow().len() != *n as usize {
                    addtrace!();
                    handle_error!(rterr!(
                        "Expected {} values, but got a list with {} values",
                        n,
                        list.borrow().len(),
                    ));
                }
                for item in list.borrow().iter() {
                    stack.push(item.clone());
                }
            } else {
                let genobj = get0!(elements.expect_genobj());
                let mut genobj = genobj.0.borrow_mut();
                let vec = get0!(genobj.to_vec(scope, handler));
                if vec.len() != *n as usize {
                    addtrace!();
                    handle_error!(rterr!(
                        "Expected {} values, but got {} values",
                        n,
                        vec.len()
                    ));
                }
                stack.extend(vec);
            }
        }
        Opcode::Get(vscope, index) => {
            let val = scope.get(*vscope, *index).clone();
            if let Val::Invalid = val {
                addtrace!();
                handle_error!(rterr!(
                    "Variable {} used before being set",
                    scope.get_name(*vscope, *index)
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
        Opcode::AddTry(pos) => {
            trystack.push((scope.tracelen() as u32, *pos));
        }
        Opcode::PopTry => {
            trystack.pop();
        }
        Opcode::Throw => {
            let exc = stack.pop().unwrap();
            handle_error!(exc);
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
            let genobj = stack.last().unwrap().clone();
            let genobj = get0!(genobj.expect_genobj());
            let mut genobj = genobj.0.borrow_mut();
            match get1!(genobj.resume(scope, handler, Val::Nil)) {
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
                addtrace!();
                handle_error!(rterr!("{} is not a function", func));
            };

            if func.generator() {
                // generator; create a generator object
                let genobj = get0!(mkgenobj(func.clone(), args));
                stack.push(genobj);
            } else {
                // this is a normal function, call it
                let ret = get1!(callfunc(scope, handler, &func, args));
                stack.push(ret);
            }
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
                        addtrace!();
                        handle_error!(rterr!(
                            concat!(
                                "The left hand side of this arithmetic operation ",
                                "should be a number but got {:?}"
                            ),
                            lhs
                        ));
                    };
                    let rhs = if let Some(rhs) = rhs.number() {
                        rhs
                    } else {
                        addtrace!();
                        handle_error!(rterr!(
                            concat!(
                                "The right hand side of this arithmetic operation ",
                                "should be a number but got {:?}"
                            ),
                            rhs
                        ));
                    };
                    match aop {
                        ArithmeticBinop::Add => Val::Number(lhs + rhs),
                        ArithmeticBinop::Subtract => Val::Number(lhs - rhs),
                        ArithmeticBinop::Multiply => Val::Number(lhs * rhs),
                        ArithmeticBinop::Divide => Val::Number(lhs / rhs),
                        ArithmeticBinop::TruncDivide => Val::Number((lhs / rhs).trunc()),
                        ArithmeticBinop::Remainder => Val::Number(lhs % rhs),
                        ArithmeticBinop::Exponentiate => Val::Number(lhs.powf(rhs)),
                        ArithmeticBinop::ATan2 => Val::Number(lhs.atan2(rhs)),
                    }
                }

                // comparison operators
                Binop::Is => Val::Bool(lhs.is(&rhs)),
                Binop::IsNot => Val::Bool(!lhs.is(&rhs)),
                Binop::Equal => Val::Bool(lhs == rhs),
                Binop::NotEqual => Val::Bool(lhs != rhs),
                Binop::LessThan => Val::Bool(get0!(lhs.lt(&rhs))),
                Binop::LessThanOrEqual => Val::Bool(!get0!(rhs.lt(&lhs))),
                Binop::GreaterThan => Val::Bool(get0!(rhs.lt(&lhs))),
                Binop::GreaterThanOrEqual => Val::Bool(!get0!(lhs.lt(&rhs))),
                Binop::In => Val::Bool(get0!(lhs.in_(&rhs))),
                Binop::NotIn => Val::Bool(!get0!(lhs.in_(&rhs))),

                // other
                Binop::Add => {
                    match &lhs {
                        Val::List(list) => {
                            list.borrow_mut().push(rhs);
                        }
                        Val::Set(set) => {
                            let key = match Key::from_val(rhs) {
                                Ok(key) => key,
                                Err(val) => {
                                    addtrace!();
                                    handle_error!(rterr!("{:?} is not hashable", val));
                                }
                            };
                            set.borrow_mut().insert(key);
                        }
                        Val::Map(map) => {
                            let (key, val) = match rhs.try_key_val_pair() {
                                Some(pair) => pair,
                                None => {
                                    addtrace!();
                                    handle_error!(rterr!(
                                        "{:?} is not a proper key-value pair",
                                        rhs
                                    ));
                                }
                            };
                            map.borrow_mut().insert(key, val);
                        }
                        _ => {
                            addtrace!();
                            handle_error!(rterr!("Cannot ADD to a {:?}", lhs.type_()));
                        }
                    }
                    lhs
                }
                Binop::GetItem => match lhs {
                    Val::String(string) => {
                        let len = string.charlen();
                        let j = get0!(index(&rhs, len));
                        format!("{}", string.getchar(j).unwrap()).into()
                    }
                    Val::List(list) => {
                        let len = list.borrow().len();
                        let index = get0!(index(&rhs, len));
                        list.borrow()[index].clone()
                    }
                    Val::Map(map) => {
                        let key = match Key::from_val(rhs) {
                            Ok(key) => key,
                            Err(rhs) => {
                                addtrace!();
                                handle_error!(rterr!("{:?} is not hashable", rhs));
                            }
                        };
                        match map.borrow().get(&key).cloned() {
                            Some(val) => val,
                            None => {
                                addtrace!();
                                handle_error!(rterr!("Key not present in the given map"));
                            }
                        }
                    }
                    lhs => {
                        addtrace!();
                        handle_error!(rterr!(
                            concat!(
                                "GETITEM requries its first element to be a list, ",
                                "string, or map but got {:?}",
                            ),
                            lhs
                        ));
                    }
                },
                Binop::Remove => match lhs {
                    Val::List(list) => {
                        let j = get0!(index(&rhs, list.borrow().len()));
                        list.borrow_mut().remove(j)
                    }
                    Val::Set(set) => {
                        let key = get0!(getkey(rhs));
                        set.borrow_mut().remove(&key).into()
                    }
                    Val::Map(map) => {
                        let key = get0!(getkey(rhs));
                        map.borrow_mut().remove(&key).unwrap_or(Val::Nil)
                    }
                    _ => {
                        addtrace!();
                        handle_error!(rterr!("Cannot REMOVE from a {:?}", lhs.type_()));
                    }
                },
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
                        addtrace!();
                        handle_error!(rterr!(
                            concat!(
                                "The argument to this unary arithmetic operator ",
                                "should be a number but got {:?}"
                            ),
                            val
                        ));
                    };
                    match aop {
                        ArithmeticUnop::Negative => Val::Number(-val),
                        ArithmeticUnop::Positive => Val::Number(val),
                        ArithmeticUnop::Sin => Val::Number(val.sin()),
                        ArithmeticUnop::Cos => Val::Number(val.cos()),
                        ArithmeticUnop::Tan => Val::Number(val.tan()),
                        ArithmeticUnop::ASin => Val::Number(val.asin()),
                        ArithmeticUnop::ACos => Val::Number(val.acos()),
                        ArithmeticUnop::ATan => Val::Number(val.atan()),
                    }
                }
                Unop::Type => Val::Type(val.type_()),
                Unop::Len => match val {
                    Val::String(s) => Val::Number(s.charlen() as f64),
                    Val::List(list) => Val::Number(list.borrow().len() as f64),
                    Val::Set(set) => Val::Number(set.borrow().len() as f64),
                    Val::Map(map) => Val::Number(map.borrow().len() as f64),
                    _ => {
                        addtrace!();
                        handle_error!(rterr!(
                            concat!("LEN requires a string, list, set or map argument but got {}"),
                            val,
                        ));
                    }
                },
                Unop::Pop => {
                    let list = get0!(val.expect_list());
                    match list.borrow_mut().pop() {
                        Some(val) => val,
                        None => {
                            addtrace!();
                            handle_error!(rterr!("Pop from empty list"));
                        }
                    }
                }
                Unop::Name => match val {
                    Val::Type(type_) => format!("{:?}", type_).into(),
                    Val::Func(func) => func.0.name().into(),
                    _ => {
                        addtrace!();
                        handle_error!(rterr!(
                            concat!("NAME requires a function argument but got {}"),
                            val,
                        ));
                    }
                },
                Unop::Str => format!("{}", val).into(),
                Unop::Repr => format!("{:?}", val).into(),
                Unop::Not => Val::Bool(!val.truthy()),
                Unop::Cat => {
                    let mut string = String::new();
                    cat(&mut string, &val);
                    string.into()
                }
                Unop::Sort => {
                    let list = get0!(val.expect_list());
                    get0!(sort(&mut list.borrow_mut()));
                    Val::Nil
                }
            };
            stack.push(ret);
        }
        Opcode::SetItem => {
            let j = stack.pop().unwrap();
            let owner = stack.pop().unwrap();
            let val = stack.pop().unwrap();
            match owner {
                Val::List(list) => {
                    let len = list.borrow().len();
                    let j = get0!(index(&j, len));
                    list.borrow_mut()[j] = val;
                }
                Val::Map(map) => {
                    let key = match Key::from_val(j) {
                        Ok(key) => key,
                        Err(j) => {
                            addtrace!();
                            handle_error!(rterr!("{:?} is not hashable", j));
                        }
                    };
                    map.borrow_mut().insert(key, val);
                }
                lhs => {
                    addtrace!();
                    handle_error!(rterr!(
                        concat!(
                            "SETITEM requries its first element to be a list ",
                            "or map but got {:?}",
                        ),
                        lhs
                    ));
                }
            }
        }
        Opcode::Print => {
            let x = stack.pop().unwrap();
            get0!(handler.print(scope, x));
        }
        Opcode::Disasm => {
            let f = stack.pop().unwrap();
            if let Val::Func(func) = &f {
                stack.push(func.0.format().into());
            } else {
                addtrace!();
                handle_error!(
                    rterr!(concat!("DISASM requires a function argument but got {}"), f,).into(),
                );
            }
        }
        Opcode::AddToTest => {
            let val = stack.last().unwrap().clone();
            if let Val::Func(code) = &val {
                scope.tests.push(code.0.clone());
            } else {
                addtrace!();
                handle_error!(rterr!(
                    concat!("Tests need to be functions, but {} is not a function"),
                    val,
                ));
            }
        }
        Opcode::Assert => {
            let val = stack.pop().unwrap();
            if !val.truthy() {
                addtrace!();
                handle_error!(rterr!(concat!("Assertion failed")));
            }
        }
        Opcode::AssertBinop(op) => {
            let rhs = stack.pop().unwrap();
            let lhs = stack.pop().unwrap();
            let (cond, msg) = match op {
                AssertBinop::Is => (lhs.is(&rhs), " to have same identity as "),
                AssertBinop::IsNot => (!lhs.is(&rhs), " to have distinct identity from "),
                AssertBinop::Equal => (lhs == rhs, " to equal "),
                AssertBinop::NotEqual => (lhs != rhs, " to not equal "),
            };
            if !cond {
                addtrace!();
                return Err(rterr!(
                    "Assertion failed: expected {:?}{}{:?}",
                    lhs,
                    msg,
                    rhs,
                ));
            }
        }
        Opcode::AssertThrowFailed => {
            addtrace!();
            return Err(rterr!(
                "Assertion failed: exception not thrown"
            ))
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
        Opcode::GotoIfFalseElsePop(pos) => {
            let item = stack.last().unwrap();
            if !item.truthy() {
                *i = *pos as usize;
            } else {
                stack.pop().unwrap();
            }
        }
        Opcode::GotoIfTrueElsePop(pos) => {
            let item = stack.last().unwrap();
            if item.truthy() {
                *i = *pos as usize;
            } else {
                stack.pop().unwrap();
            }
        }
        Opcode::Label(_)
        | Opcode::UnresolvedAddTry(_)
        | Opcode::UnresolvedGoto(_)
        | Opcode::UnresolvedGotoIfFalse(_)
        | Opcode::UnresolvedGotoIfFalseElsePop(_)
        | Opcode::UnresolvedGotoIfTrueElsePop(_) => {
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
    tests: Vec<Rc<Code>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            globals: IndexedMap::new(),
            locals: vec![],
            trace: vec![],
            tests: vec![],
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
    pub fn pop(&mut self) -> IndexedMap {
        self.locals.pop().unwrap()
    }
    pub fn tracelen(&self) -> usize {
        self.trace.len()
    }
    pub fn trunc_tracelen(&mut self, new_len: usize) {
        self.trace.truncate(new_len);
        assert_eq!(self.trace.len(), new_len);
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
        let index = map.insert(var.name.clone(), Val::Invalid);
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
    pub fn set_by_index(&mut self, i: u32, val: Val) {
        self.values[i as usize] = val;
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
    trystack: Vec<(u32, u32)>,
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
        self.locals = scope.pop();
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
            match step(
                scope,
                handler,
                &self.code,
                &mut self.i,
                &mut self.stack,
                &mut self.trystack,
            )? {
                StepVal::None => {}
                StepVal::Return(_) => {
                    self.i = self.code.len();
                    self.clear(); // release all local vars etc
                    return Ok(None);
                }
                StepVal::Yield(val) => return Ok(Some(val)),
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

fn index(i: &Val, len: usize) -> Result<usize, Val> {
    match i {
        Val::Number(i) => {
            let mut i = *i as i64;
            if i < 0 {
                i += len as i64;
            }
            if i < 0 || i >= len as i64 {
                Err(rterr!("Index out of bounds (i = {}, len = {})", i, len))
            } else {
                Ok(i as usize)
            }
        }
        _ => Err(rterr!("Expected index, but got {:?}", i)),
    }
}

pub fn rterr<S: Into<RcStr>>(message: S) -> Val {
    let vec: Vec<Val> = vec!["RuntimeError".into(), message.into().into()];
    vec.into()
}

fn cat(out: &mut String, val: &Val) {
    use std::fmt::Write;
    match val {
        Val::List(list) => {
            for x in list.borrow().iter() {
                cat(out, x);
            }
        }
        Val::Set(set) => {
            for key in set.borrow().iter() {
                cat(out, &key.clone().to_val());
            }
        }
        Val::Map(map) => {
            for (k, v) in map.borrow().iter() {
                cat(out, &k.clone().to_val());
                cat(out, v);
            }
        }
        _ => write!(out, "{}", val).unwrap(),
    }
}

fn getkey(val: Val) -> Result<Key, Val> {
    match Key::from_val(val) {
        Ok(key) => Ok(key),
        Err(val) => Err(rterr!("{:?} is not hashable", val)),
    }
}

fn sort(vec: &mut Vec<Val>) -> Result<(), Val> {
    use std::cmp::Ordering;
    let mut err = None;
    vec.sort_by(|a, b| {
        if err.is_some() {
            Ordering::Equal
        } else {
            match a.cmp(b) {
                Ok(ord) => ord,
                Err(e) => {
                    err = Some(e);
                    Ordering::Equal
                }
            }
        }
    });
    err.map(Err).unwrap_or(Ok(()))
}
