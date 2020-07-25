use super::ast::*;
use super::ArgSpec;
use super::ArithmeticBinop;
use super::BasicError;
use super::Binop;
use super::Code;
use super::Opcode;
use super::RcStr;
use super::Unop;
use super::VarScope;
use super::PRELUDE_NAME;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

pub fn translate_files(mut files: Vec<File>) -> Result<Code, BasicError> {
    // enumerate all variables in files and functions,
    // compute full/unique names for all variables and functions
    let mut global_vars = Vars::new();
    for file in &mut files {
        prepare_vars_for_file(&mut global_vars, file)?;
    }

    // initialize scope with all global variables
    let mut scope = Scope::new();
    for file in &files {
        for imp in &file.imports {
            scope.decl(Item::Import(imp.clone()))?;
        }
    }
    for var in &global_vars.list {
        scope.decl(Item::Var(var.clone()))?;
    }

    let mut code = Code::new(
        false,
        format!("[main]").into(),
        ArgSpec::empty(),
        global_vars.list,
    );

    // translate all statements inside functions and
    // initialize all functions at the global scope
    for file in &files {
        scope.file_name = file.name().clone();
        for func in &file.funcs {
            let func_code = translate_func(&mut scope, func)?;
            code.add(Opcode::NewFunc(Rc::new(func_code)), func.mark.clone());
            if func.test {
                code.add(Opcode::AddToTest, func.mark.clone());
            }
            let var = func.as_var.as_ref().unwrap();
            code.add(Opcode::Set(var.vscope, var.index), func.mark.clone());
        }
    }

    // translate all other global statements
    for file in &files {
        scope.file_name = file.name().clone();
        translate_stmt(&mut code, &mut scope, &file.body)?;
    }

    code.resolve_labels()?;

    Ok(code)
}

struct Vars {
    list: Vec<Var>,
    map: HashMap<RcStr, Var>,
}

impl Vars {
    fn new() -> Self {
        Self {
            list: vec![],
            map: HashMap::new(),
        }
    }
    fn add(&mut self, var: Var) {
        if !self.map.contains_key(&var.name) {
            self.force_add(var).unwrap();
        }
    }
    fn force_add(&mut self, var: Var) -> Result<(), BasicError> {
        assert_eq!(self.list.len(), self.map.len());
        if let Some(oldvar) = self.map.get(&var.name) {
            Err(BasicError::new_with_help(
                vec![var.mark.clone(), oldvar.mark.clone()],
                format!("Conflicting definitions of {}", var.name),
                format!(concat!(
                    "To reduce the chance of confusion, it is considered an ",
                    "error for a function to share a name with a normal variable ",
                    "or even another function.",
                )),
            ))
        } else {
            self.map.insert(var.name.clone(), var.clone());
            self.list.push(var);
            Ok(())
        }
    }
    fn len(&self) -> usize {
        assert_eq!(self.list.len(), self.map.len());
        self.list.len()
    }
}

fn prepare_vars_for_file(out: &mut Vars, file: &mut File) -> Result<(), BasicError> {
    let file_name = file.name().clone();

    for imp in &mut file.imports {
        imp.unique_name = format!("{}#{}", file_name, imp.alias).into();
    }

    prepare_vars_for_stmt(out, &file.body, Some(&file_name))?;

    // while we allow variables to be assigned to multiple times,
    // we disallow functions to share names with normal variables
    // or even other functions.
    //
    // To enforce this, we add the function variables last, and
    // raise an error if we encounter any conflicts

    for func in &mut file.funcs {
        prepare_vars_for_func(func)?;
        let var = mkvar(
            func.mark.clone(),
            &func.short_name,
            Some(&file_name),
            out.len(),
        );
        func.as_var = Some(var.clone());
        out.force_add(var)?;
    }

    Ok(())
}

fn prepare_vars_for_func(func: &mut FuncDisplay) -> Result<(), BasicError> {
    let mut vars = Vars::new();
    let spec = &func.argspec;
    for param in &spec.req {
        let var = mkvar(func.mark.clone(), param, None, vars.len());
        vars.force_add(var)?;
    }
    for (param, _) in &spec.def {
        let var = mkvar(func.mark.clone(), param, None, vars.len());
        vars.force_add(var)?;
    }
    if let Some(param) = &spec.var {
        let var = mkvar(func.mark.clone(), param, None, vars.len());
        vars.force_add(var)?;
    }
    prepare_vars_for_stmt(&mut vars, &func.body, None)?;
    func.vars = vars.list;
    Ok(())
}

fn prepare_vars_for_stmt(
    out: &mut Vars,
    stmt: &Stmt,
    prefix: Option<&RcStr>,
) -> Result<(), BasicError> {
    match &stmt.desc {
        StmtDesc::Block(stmts) => {
            for stmt in stmts {
                prepare_vars_for_stmt(out, stmt, prefix)?;
            }
        }
        StmtDesc::Return(expr) => {
            if let Some(expr) = expr {
                prepare_vars_for_expr(out, expr, prefix)?;
            }
        }
        StmtDesc::Assign(target, other_targets, expr) => {
            prepare_vars_for_target(out, target, prefix)?;
            for target in other_targets {
                prepare_vars_for_target(out, target, prefix)?;
            }
            prepare_vars_for_expr(out, expr, prefix)?;
        }
        StmtDesc::Expr(expr) => {
            prepare_vars_for_expr(out, expr, prefix)?;
        }
        StmtDesc::Print(expr) => {
            prepare_vars_for_expr(out, expr, prefix)?;
        }
        StmtDesc::Assert(expr) => {
            prepare_vars_for_expr(out, expr, prefix)?;
        }
        StmtDesc::AssertThrow(assert_stmt) => {
            prepare_vars_for_stmt(out, assert_stmt, prefix)?;
        }
        StmtDesc::If(pairs, other) => {
            for (_cond, body) in pairs {
                prepare_vars_for_stmt(out, body, prefix)?;
            }
            if let Some(other) = other {
                prepare_vars_for_stmt(out, other, prefix)?;
            }
        }
        StmtDesc::While(cond, body) => {
            prepare_vars_for_expr(out, cond, prefix)?;
            prepare_vars_for_stmt(out, body, prefix)?;
        }
        StmtDesc::ForIn(target, container, body) => {
            prepare_vars_for_target(out, target, prefix)?;
            prepare_vars_for_expr(out, container, prefix)?;
            prepare_vars_for_stmt(out, body, prefix)?;
        }
        StmtDesc::ForClassic(target, start, end, _inclusive, _step, body) => {
            prepare_vars_for_target(out, target, prefix)?;
            prepare_vars_for_expr(out, start, prefix)?;
            prepare_vars_for_expr(out, end, prefix)?;
            prepare_vars_for_stmt(out, body, prefix)?;
        }
        StmtDesc::Try(body, target, onerr) => {
            prepare_vars_for_stmt(out, body, prefix)?;
            prepare_vars_for_target(out, target, prefix)?;
            prepare_vars_for_stmt(out, onerr, prefix)?;
        }
        StmtDesc::Throw(expr) => {
            prepare_vars_for_expr(out, expr, prefix)?;
        }
        StmtDesc::Label(_)
        | StmtDesc::Goto(_) => {}
    }
    Ok(())
}

fn prepare_vars_for_expr(
    out: &mut Vars,
    expr: &Expr,
    prefix: Option<&RcStr>,
) -> Result<(), BasicError> {
    match &expr.desc {
        ExprDesc::Nil => {}
        ExprDesc::Bool(_) => {}
        ExprDesc::Number(_) => {}
        ExprDesc::String(_) => {}
        ExprDesc::List(exprs) => {
            for expr in exprs {
                prepare_vars_for_expr(out, expr, prefix)?;
            }
        }
        ExprDesc::Set(exprs) => {
            for expr in exprs {
                prepare_vars_for_expr(out, expr, prefix)?;
            }
        }
        ExprDesc::Map(pairs) => {
            for (key, val) in pairs {
                prepare_vars_for_expr(out, key, prefix)?;
                prepare_vars_for_expr(out, val, prefix)?;
            }
        }
        ExprDesc::GetVar(_) => {}
        ExprDesc::GetAttr(owner, _) => {
            prepare_vars_for_expr(out, owner, prefix)?;
        }
        ExprDesc::CallFunc(f, args) => {
            prepare_vars_for_expr(out, f, prefix)?;
            for expr in args {
                prepare_vars_for_expr(out, expr, prefix)?;
            }
        }
        ExprDesc::Binop(_, a, b) => {
            prepare_vars_for_expr(out, a, prefix)?;
            prepare_vars_for_expr(out, b, prefix)?;
        }
        ExprDesc::Unop(_, arg) => {
            prepare_vars_for_expr(out, arg, prefix)?;
        }
        ExprDesc::And(a, b) => {
            prepare_vars_for_expr(out, a, prefix)?;
            prepare_vars_for_expr(out, b, prefix)?;
        }
        ExprDesc::Or(a, b) => {
            prepare_vars_for_expr(out, a, prefix)?;
            prepare_vars_for_expr(out, b, prefix)?;
        }
        ExprDesc::If(cond, body, other) => {
            prepare_vars_for_expr(out, cond, prefix)?;
            prepare_vars_for_expr(out, body, prefix)?;
            prepare_vars_for_expr(out, other, prefix)?;
        }
        ExprDesc::Yield(arg) => {
            prepare_vars_for_expr(out, arg, prefix)?;
        }
        ExprDesc::Next(arg) => {
            prepare_vars_for_expr(out, arg, prefix)?;
        }
        ExprDesc::Cat(args) => {
            for expr in args {
                prepare_vars_for_expr(out, expr, prefix)?;
            }
        }
        ExprDesc::Disasm(arg) => {
            prepare_vars_for_expr(out, arg, prefix)?;
        }
    }
    Ok(())
}

fn prepare_vars_for_target(
    out: &mut Vars,
    target: &AssignTarget,
    prefix: Option<&RcStr>,
) -> Result<(), BasicError> {
    match &target.desc {
        AssignTargetDesc::Name(name) => {
            out.add(mkvar(target.mark.clone(), name, prefix, out.len()));
        }
        AssignTargetDesc::List(list) => {
            for subtarget in list {
                prepare_vars_for_target(out, subtarget, prefix)?;
            }
        }
        AssignTargetDesc::Subscript(..) => {}
    }
    Ok(())
}

fn mkvar(mark: Mark, name: &RcStr, file_name: Option<&RcStr>, index: usize) -> Var {
    let index = index as u32;
    if let Some(file_name) = file_name {
        Var {
            mark: mark,
            name: format!("{}#{}", file_name, name).into(),
            vscope: VarScope::Global,
            index,
        }
    } else {
        Var {
            mark: mark,
            name: name.clone(),
            vscope: VarScope::Local,
            index,
        }
    }
}

fn translate_func(scope: &mut Scope, func: &FuncDisplay) -> Result<Code, BasicError> {
    let mut code = Code::new(
        func.generator,
        func.full_name().clone(),
        func.argspec.clone(),
        func.vars.clone(),
    );
    scope.enter_local();
    for var in &func.vars {
        scope.decl(Item::Var(var.clone()))?;
    }
    translate_stmt(&mut code, scope, &func.body)?;
    scope.exit_local();
    code.resolve_labels()?;
    Ok(code)
}

fn translate_stmt(code: &mut Code, scope: &mut Scope, stmt: &Stmt) -> Result<(), BasicError> {
    match &stmt.desc {
        StmtDesc::Block(stmts) => {
            for child_stmt in stmts {
                translate_stmt(code, scope, child_stmt)?;
            }
        }
        StmtDesc::Return(expr) => {
            if let Some(expr) = expr {
                translate_expr(code, scope, expr)?;
            } else {
                code.add(Opcode::Nil, stmt.mark.clone());
            }
            code.add(Opcode::Return, stmt.mark.clone());
        }
        StmtDesc::Assign(target, other_targets, expr) => {
            translate_expr(code, scope, expr)?;
            for target in other_targets.iter().rev() {
                translate_assign(code, scope, target, false)?;
            }
            translate_assign(code, scope, target, true)?;
        }
        StmtDesc::Expr(expr) => {
            translate_expr(code, scope, expr)?;
            code.add(Opcode::Pop, stmt.mark.clone());
        }
        StmtDesc::Print(arg) => {
            translate_expr(code, scope, arg)?;
            code.add(Opcode::Print, stmt.mark.clone());
        }
        StmtDesc::Assert(arg) => match &arg.desc {
            ExprDesc::Binop(op, lhs, rhs) if op.to_assert().is_some() => {
                let aop = op.to_assert().unwrap();
                translate_expr(code, scope, lhs)?;
                translate_expr(code, scope, rhs)?;
                code.add(Opcode::AssertBinop(aop), stmt.mark.clone());
            }
            _ => {
                translate_expr(code, scope, arg)?;
                code.add(Opcode::Assert, stmt.mark.clone());
            }
        },
        StmtDesc::AssertThrow(assert_stmt) => {
            let catch_label = scope.new_label();
            code.add(
                Opcode::UnresolvedAddTry(catch_label.clone()),
                stmt.mark.clone(),
            );
            translate_stmt(code, scope, assert_stmt)?;
            code.add(Opcode::PopTry, stmt.mark.clone());
            code.add(Opcode::AssertThrowFailed, stmt.mark.clone());
            code.add(Opcode::Label(catch_label), stmt.mark.clone());
            code.add(Opcode::Pop, stmt.mark.clone());
        }
        StmtDesc::Label(label) => {
            code.add(Opcode::Label(label.clone()), stmt.mark.clone());
        }
        StmtDesc::Goto(label) => {
            code.add(Opcode::UnresolvedGoto(label.clone()), stmt.mark.clone());
        }
        StmtDesc::If(pairs, other) => {
            let end_label = scope.new_label();
            for (cond, body) in pairs {
                let next_label = scope.new_label();
                translate_expr(code, scope, cond)?;
                code.add(
                    Opcode::UnresolvedGotoIfFalse(next_label.clone()),
                    cond.mark.clone(),
                );
                translate_stmt(code, scope, body)?;
                code.add(Opcode::UnresolvedGoto(end_label.clone()), body.mark.clone());
                code.add(Opcode::Label(next_label), cond.mark.clone());
            }
            if let Some(other) = other {
                translate_stmt(code, scope, other)?;
            }
            code.add(Opcode::Label(end_label), stmt.mark.clone());
        }
        StmtDesc::While(cond, body) => {
            let start_label = scope.new_label();
            let end_label = scope.new_label();
            code.add(Opcode::Label(start_label.clone()), stmt.mark.clone());
            translate_expr(code, scope, cond)?;
            code.add(
                Opcode::UnresolvedGotoIfFalse(end_label.clone()),
                cond.mark.clone(),
            );
            translate_stmt(code, scope, body)?;
            code.add(Opcode::UnresolvedGoto(start_label), stmt.mark.clone());
            code.add(Opcode::Label(end_label), stmt.mark.clone());
        }
        StmtDesc::ForIn(target, container, body) => {
            let start_label = scope.new_label();
            let end_label = scope.new_label();

            translate_expr(code, scope, container)?;

            code.add(Opcode::Label(start_label.clone()), stmt.mark.clone());
            code.add(Opcode::Next, stmt.mark.clone());
            code.add(
                Opcode::UnresolvedGotoIfFalse(end_label.clone()),
                stmt.mark.clone(),
            );

            translate_assign(code, scope, target, true)?;
            translate_stmt(code, scope, body)?;
            code.add(Opcode::UnresolvedGoto(start_label), stmt.mark.clone());

            code.add(Opcode::Label(end_label), stmt.mark.clone());
            code.add(Opcode::Pop, stmt.mark.clone()); // pop nil
            code.add(Opcode::Pop, stmt.mark.clone()); // pop genobj
        }
        StmtDesc::ForClassic(target, start, end, inclusive, step, body) => {
            let start_label = scope.new_label();
            let end_label = scope.new_label();

            translate_expr(code, scope, end)?;
            translate_expr(code, scope, start)?;

            code.add(Opcode::Label(start_label.clone()), stmt.mark.clone());

            // Test that start <-> end
            // NOTE: start is at TOS, so operators need to be 'flipped'
            code.add(Opcode::Dup2, stmt.mark.clone());
            code.add(
                Opcode::Binop(if *step < 0.0 {
                    if *inclusive {
                        Binop::LessThanOrEqual
                    } else {
                        Binop::LessThan
                    }
                } else if *step > 0.0 {
                    if *inclusive {
                        Binop::GreaterThanOrEqual
                    } else {
                        Binop::GreaterThan
                    }
                } else {
                    return Err(BasicError::new(
                        vec![stmt.mark.clone()],
                        format!("The STEP of a FOR loop cannot be zero"),
                    ));
                }),
                stmt.mark.clone(),
            );
            code.add(
                Opcode::UnresolvedGotoIfFalse(end_label.clone()),
                stmt.mark.clone(),
            );

            // assign 'start' to 'target'
            translate_assign(code, scope, target, false)?;

            translate_stmt(code, scope, body)?;

            code.add(Opcode::Number(*step), stmt.mark.clone());
            code.add(
                Opcode::Binop(Binop::Arithmetic(ArithmeticBinop::Add)),
                stmt.mark.clone(),
            );
            code.add(Opcode::UnresolvedGoto(start_label), stmt.mark.clone());

            code.add(Opcode::Label(end_label), stmt.mark.clone());
            code.add(Opcode::Pop, stmt.mark.clone()); // pop start
            code.add(Opcode::Pop, stmt.mark.clone()); // pop end
        }
        StmtDesc::Try(body, target, onerr) => {
            let catch_label = scope.new_label();
            let end_label = scope.new_label();

            // main body
            // set up a try, and if successful just jump to the end
            code.add(
                Opcode::UnresolvedAddTry(catch_label.clone()),
                stmt.mark.clone(),
            );
            translate_stmt(code, scope, body)?;
            code.add(Opcode::PopTry, stmt.mark.clone());
            code.add(Opcode::UnresolvedGoto(end_label.clone()), stmt.mark.clone());

            // catch clause
            code.add(Opcode::Label(catch_label), stmt.mark.clone());
            translate_assign(code, scope, target, true)?;
            translate_stmt(code, scope, onerr)?;

            // Jump location after main body finishes
            code.add(Opcode::Label(end_label), stmt.mark.clone());
        }
        StmtDesc::Throw(expr) => {
            translate_expr(code, scope, expr)?;
            code.add(Opcode::Throw, stmt.mark.clone());
        }
    }
    Ok(())
}

/// When called, assumes the value to assign to the variable is on the top of the stack
/// the 'consume' flag whether the top of the stack value will be consumed
fn translate_assign(
    code: &mut Code,
    scope: &mut Scope,
    target: &AssignTarget,
    consume: bool,
) -> Result<(), BasicError> {
    match &target.desc {
        AssignTargetDesc::Name(name) => {
            let var = scope.getvar_or_error(&target.mark, name)?;
            if consume {
                code.add(Opcode::Set(var.vscope, var.index), target.mark.clone());
            } else {
                code.add(Opcode::Tee(var.vscope, var.index), target.mark.clone());
            }
        }
        AssignTargetDesc::List(list) => {
            if !consume {
                code.add(Opcode::Dup, target.mark.clone());
            }
            code.add(Opcode::Unpack(list.len() as u32), target.mark.clone());
            for subtarget in list.iter().rev() {
                translate_assign(code, scope, subtarget, true)?;
            }
        }
        AssignTargetDesc::Subscript(owner, index) => {
            if !consume {
                code.add(Opcode::Dup, target.mark.clone());
            }
            translate_expr(code, scope, owner)?;
            translate_expr(code, scope, index)?;
            code.add(Opcode::SetItem, target.mark.clone());
        }
    }
    Ok(())
}

fn translate_expr(code: &mut Code, scope: &mut Scope, expr: &Expr) -> Result<(), BasicError> {
    match &expr.desc {
        ExprDesc::Nil => code.add(Opcode::Nil, expr.mark.clone()),
        ExprDesc::Bool(x) => code.add(Opcode::Bool(*x), expr.mark.clone()),
        ExprDesc::Number(x) => code.add(Opcode::Number(*x), expr.mark.clone()),
        ExprDesc::String(x) => code.add(Opcode::String(x.clone()), expr.mark.clone()),
        ExprDesc::List(items) => {
            for item in items {
                translate_expr(code, scope, item)?;
            }
            code.add(Opcode::MakeList(items.len() as u32), expr.mark.clone());
        }
        ExprDesc::Set(items) => {
            for item in items {
                translate_expr(code, scope, item)?;
            }
            code.add(Opcode::MakeSet(items.len() as u32), expr.mark.clone());
        }
        ExprDesc::Map(items) => {
            for (key, val) in items {
                translate_expr(code, scope, key)?;
                translate_expr(code, scope, val)?;
            }
            code.add(Opcode::MakeMap(items.len() as u32), expr.mark.clone());
        }
        ExprDesc::GetVar(name) => {
            let var = scope.getvar_or_error(&expr.mark, name)?;
            code.add(Opcode::Get(var.vscope, var.index), expr.mark.clone());
        }
        ExprDesc::GetAttr(owner, attr) => {
            let imp = match &owner.desc {
                ExprDesc::GetVar(owner_name) => match scope.rget(owner_name) {
                    Some(Item::Import(imp)) => Some(imp),
                    _ => None,
                },
                _ => None,
            };
            if let Some(imp) = imp {
                // this is accessing an imported variable
                let full_name = format!("{}#{}", imp.module_name, attr);
                let var = scope.getvar_or_error(&expr.mark, &full_name)?;
                code.add(Opcode::Get(var.vscope, var.index), expr.mark.clone());
            } else {
                // this is an attribute access
                return Err(BasicError::new(
                    vec![expr.mark.clone()],
                    format!("Attribute access not yet supported"),
                ));
            }
        }
        ExprDesc::CallFunc(f, args) => {
            translate_expr(code, scope, f)?;
            for arg in args {
                translate_expr(code, scope, arg)?;
            }
            code.add(Opcode::CallFunc(args.len() as u32), expr.mark.clone());
        }
        ExprDesc::Binop(binop, lhs, rhs) => {
            translate_expr(code, scope, lhs)?;
            translate_expr(code, scope, rhs)?;
            code.add(Opcode::Binop(*binop), expr.mark.clone());
        }
        ExprDesc::Unop(unop, subexpr) => {
            translate_expr(code, scope, subexpr)?;
            code.add(Opcode::Unop(*unop), expr.mark.clone());
        }
        ExprDesc::Or(a, b) => {
            let end_label = scope.new_label();
            translate_expr(code, scope, a)?;
            code.add(
                Opcode::UnresolvedGotoIfTrueElsePop(end_label.clone()),
                expr.mark.clone(),
            );
            translate_expr(code, scope, b)?;
            code.add(Opcode::Label(end_label), expr.mark.clone());
        }
        ExprDesc::And(a, b) => {
            let end_label = scope.new_label();
            translate_expr(code, scope, a)?;
            code.add(
                Opcode::UnresolvedGotoIfFalseElsePop(end_label.clone()),
                expr.mark.clone(),
            );
            translate_expr(code, scope, b)?;
            code.add(Opcode::Label(end_label), expr.mark.clone());
        }
        ExprDesc::If(cond, body, other) => {
            let end_label = scope.new_label();
            let other_label = scope.new_label();
            translate_expr(code, scope, cond)?;
            code.add(
                Opcode::UnresolvedGotoIfFalse(other_label.clone()),
                expr.mark.clone(),
            );
            translate_expr(code, scope, body)?;
            code.add(Opcode::UnresolvedGoto(end_label.clone()), expr.mark.clone());
            code.add(Opcode::Label(other_label), expr.mark.clone());
            translate_expr(code, scope, other)?;
            code.add(Opcode::Label(end_label), expr.mark.clone());
        }
        ExprDesc::Yield(yieldexpr) => {
            translate_expr(code, scope, yieldexpr)?;
            code.add(Opcode::Yield, expr.mark.clone());
        }
        ExprDesc::Next(genexpr) => {
            translate_expr(code, scope, genexpr)?;
            code.add(Opcode::Next, expr.mark.clone());
            code.add(Opcode::MakeList(2), expr.mark.clone());
        }
        ExprDesc::Cat(exprs) => {
            for expr in exprs {
                translate_expr(code, scope, expr)?;
            }
            code.add(Opcode::MakeList(exprs.len() as u32), expr.mark.clone());
            code.add(Opcode::Unop(Unop::Cat), expr.mark.clone());
        }
        ExprDesc::Disasm(fexpr) => {
            translate_expr(code, scope, fexpr)?;
            code.add(Opcode::Disasm, expr.mark.clone());
        }
    }
    Ok(())
}

struct Scope {
    file_name: RcStr,
    globals: HashMap<RcStr, Item>,
    locals: Option<HashMap<RcStr, Item>>,
    labels: Vec<HashSet<RcStr>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            file_name: "".to_owned().into(),
            globals: HashMap::new(),
            locals: None,
            labels: vec![HashSet::new()],
        }
    }
    pub fn decl(&mut self, item: Item) -> Result<(), BasicError> {
        let map = if let Some(locals) = &mut self.locals {
            locals
        } else {
            &mut self.globals
        };
        if let Some(old_item) = map.get(item.name()) {
            Err(BasicError::new(
                vec![old_item.mark().clone(), item.mark().clone()],
                format!("{:?} is defined more than once", item.name()),
            ))
        } else {
            map.insert(item.name().clone(), item);
            Ok(())
        }
    }
    pub fn getvar_or_error(&self, mark: &Mark, name: &str) -> Result<&Var, BasicError> {
        match self.rget(name) {
            None => Err(BasicError::new(
                vec![mark.clone()],
                format!("Variable {:?} not found", name),
            )),
            Some(Item::Import(..)) => Err(BasicError::new(
                vec![mark.clone()],
                format!("{:?} is an import, not a variable", name),
            )),
            Some(Item::Var(var)) => Ok(var),
        }
    }
    pub fn rget(&self, name: &str) -> Option<&Item> {
        self.qget(name)
            .or_else(|| self.qget(&format!("{}#{}", self.file_name, name)))
            .or_else(|| self.qget(&format!("{}#{}", PRELUDE_NAME, name)))
    }
    pub fn qget(&self, qualified_name: &str) -> Option<&Item> {
        self.locals
            .as_ref()
            .and_then(|locals| locals.get(qualified_name))
            .or_else(|| self.globals.get(qualified_name))
    }
    pub fn new_label(&mut self) -> RcStr {
        let name: RcStr = format!("#{}", self.labels().len()).into();
        assert!(!self.labels().contains(&name));
        self.labels_mut().insert(name.clone());
        name
    }
    pub fn enter_local(&mut self) {
        if self.locals.is_some() {
            panic!("Scope::enter_local: already in local scope");
        }
        self.locals = Some(HashMap::new());
        self.labels.push(HashSet::new());
    }
    pub fn exit_local(&mut self) {
        if self.locals.is_none() {
            panic!("Scope::exit_local: not in local scope");
        }
        self.locals = None;
        self.labels.pop().unwrap();
    }
    pub fn labels(&self) -> &HashSet<RcStr> {
        self.labels.last().unwrap()
    }
    pub fn labels_mut(&mut self) -> &mut HashSet<RcStr> {
        self.labels.last_mut().unwrap()
    }
}

#[derive(Debug)]
enum Item {
    Var(Var),
    Import(Import),
}

impl Item {
    pub fn mark(&self) -> &Mark {
        match self {
            Self::Var(var) => &var.mark,
            Self::Import(imp) => &imp.mark,
        }
    }
    pub fn name(&self) -> &RcStr {
        match self {
            Self::Var(var) => &var.name,
            Self::Import(imp) => &imp.unique_name,
        }
    }
}
