use super::ast::*;
use super::ArgSpec;
use super::BasicError;
use super::Binop;
use super::Code;
use super::Opcode;
use super::RcStr;
use super::VarScope;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

pub fn translate_files(mut files: Vec<File>) -> Result<Code, BasicError> {
    // enumerate all variables in files and functions,
    // compute full/unique names for all variables and functions
    let mut global_vars = Vec::new();
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
    for var in &global_vars {
        scope.decl(Item::Var(var.clone()))?;
    }

    let mut code = Code::new(
        false,
        format!("[main]").into(),
        ArgSpec::empty(),
        global_vars,
    );

    // translate all statements inside functions and
    // initialize all functions at the global scope
    for file in &files {
        scope.file_name = file.name().clone();
        for func in &file.funcs {
            let func_code = translate_func(&mut scope, func)?;
            code.add(Opcode::NewFunc(Rc::new(func_code)), func.mark.clone());
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

fn prepare_vars_for_file(out: &mut Vec<Var>, file: &mut File) -> Result<(), BasicError> {
    let file_name = file.name().clone();

    for imp in &mut file.imports {
        imp.unique_name = format!("{}#{}", file_name, imp.alias).into();
    }

    for func in &mut file.funcs {
        prepare_vars_for_func(func)?;
        let var = mkvar(
            func.mark.clone(),
            &func.short_name,
            Some(&file_name),
            out.len(),
        );
        func.as_var = Some(var.clone());
        out.push(var);
    }

    prepare_vars_for_stmt(out, &mut file.body, Some(&file_name))?;
    Ok(())
}

fn prepare_vars_for_func(func: &mut FuncDisplay) -> Result<(), BasicError> {
    let mut vars = Vec::new();
    let spec = &func.argspec;
    for param in &spec.req {
        let var = mkvar(func.mark.clone(), param, None, vars.len());
        vars.push(var);
    }
    for (param, _) in &spec.def {
        let var = mkvar(func.mark.clone(), param, None, vars.len());
        vars.push(var);
    }
    if let Some(param) = &spec.var {
        let var = mkvar(func.mark.clone(), param, None, vars.len());
        vars.push(var);
    }
    prepare_vars_for_stmt(&mut vars, &mut func.body, None)?;
    func.vars = vars;
    Ok(())
}

fn prepare_vars_for_stmt(
    out: &mut Vec<Var>,
    stmt: &mut Stmt,
    prefix: Option<&RcStr>,
) -> Result<(), BasicError> {
    match &mut stmt.desc {
        StmtDesc::Block(stmts) => {
            for stmt in stmts {
                prepare_vars_for_stmt(out, stmt, prefix)?;
            }
        }
        StmtDesc::DeclVar(name, setexpr) => {
            out.push(mkvar(stmt.mark.clone(), name, prefix, out.len()));

            // convert this DeclVar into a SetVar
            let setexpr = std::mem::replace(
                setexpr,
                Expr {
                    mark: stmt.mark.clone(),
                    desc: ExprDesc::Nil,
                },
            );
            stmt.desc = StmtDesc::Expr(Expr {
                mark: stmt.mark.clone(),
                desc: ExprDesc::SetVar(name.clone(), setexpr.into()),
            });
        }
        StmtDesc::If(pairs, other) => {
            for (_cond, body) in pairs {
                prepare_vars_for_stmt(out, body, prefix)?;
            }
            if let Some(other) = other {
                prepare_vars_for_stmt(out, other, prefix)?;
            }
        }
        StmtDesc::While(_cond, body) => {
            prepare_vars_for_stmt(out, body, prefix)?;
        }
        StmtDesc::Print(_)
        | StmtDesc::Expr(_)
        | StmtDesc::Return(_)
        | StmtDesc::Label(_)
        | StmtDesc::Goto(_) => {}
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
        StmtDesc::DeclVar(..) => panic!("translate_stmt: DeclVar should've become Set"),
        StmtDesc::Expr(expr) => {
            translate_expr(code, scope, expr)?;
            code.add(Opcode::Pop, stmt.mark.clone());
        }
        StmtDesc::Print(arg) => {
            translate_expr(code, scope, arg)?;
            code.add(Opcode::Print, stmt.mark.clone());
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
            code.add(Opcode::NewList, expr.mark.clone());
            for item in items {
                translate_expr(code, scope, item)?;
                code.add(Opcode::Binop(Binop::Append), expr.mark.clone());
            }
        }
        ExprDesc::GetVar(name) => {
            let var = scope.getvar_or_error(&expr.mark, name)?;
            code.add(Opcode::Get(var.vscope, var.index), expr.mark.clone());
        }
        ExprDesc::SetVar(name, setexpr) => {
            translate_expr(code, scope, setexpr)?;
            let var = scope.getvar_or_error(&expr.mark, name)?;
            code.add(Opcode::Tee(var.vscope, var.index), expr.mark.clone());
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
                return Err(BasicError {
                    marks: vec![expr.mark.clone()],
                    message: format!("Attribute access not yet supported"),
                });
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
        ExprDesc::Yield(yieldexpr) => {
            translate_expr(code, scope, yieldexpr)?;
            code.add(Opcode::Yield, expr.mark.clone());
        }
        ExprDesc::Next(genexpr) => {
            translate_expr(code, scope, genexpr)?;
            code.add(Opcode::Next, expr.mark.clone());
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
            Err(BasicError {
                marks: vec![old_item.mark().clone(), item.mark().clone()],
                message: format!("{} is defined more than once", item.name()),
            })
        } else {
            map.insert(item.name().clone(), item);
            Ok(())
        }
    }
    pub fn getvar_or_error(&self, mark: &Mark, name: &str) -> Result<&Var, BasicError> {
        match self.rget(name) {
            None => Err(BasicError {
                marks: vec![mark.clone()],
                message: format!("Variable {} not found", name),
            }),
            Some(Item::Import(..)) => Err(BasicError {
                marks: vec![mark.clone()],
                message: format!("{} is an import, not a variable", name),
            }),
            Some(Item::Var(var)) => Ok(var),
        }
    }
    pub fn rget(&self, name: &str) -> Option<&Item> {
        self.qget(name)
            .or_else(|| self.qget(&format!("{}#{}", self.file_name, name)))
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
