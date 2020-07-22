use super::ast::*;
use super::BasicError;
use super::Binop;
use super::Code;
use super::Opcode;
use super::VarScope;
use super::INVALID_LABEL_LOC;
use super::RcStr;
use std::collections::HashMap;
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

    let mut code = Code::new(format!("[main]").into(), 0, global_vars);

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

    let label_map = scope.label_map().clone();
    let label_names = scope.label_names().clone();
    code.set_label_data(label_map, label_names);

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
    for param in &func.params {
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
        func.full_name().clone(),
        func.params.len(),
        func.vars.clone(),
    );
    scope.enter_local();
    for var in &func.vars {
        scope.decl(Item::Var(var.clone()))?;
    }
    translate_stmt(&mut code, scope, &func.body)?;
    let label_names = scope.label_names().clone();
    let label_map = scope.label_map().clone();
    code.set_label_data(label_map, label_names);
    scope.exit_local();
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
            let loc = code.len();
            scope.update_label(label, loc);
            // let id = scope.get_label_id(label);
            // code.add(Opcode::Label(id), stmt.mark.clone());
        }
        StmtDesc::Goto(label) => {
            let id = scope.get_label_id(label);
            code.add(Opcode::Goto(id), stmt.mark.clone());
        }
        StmtDesc::If(pairs, other) => {
            let end_label = scope.new_label();
            let end_label_id = scope.get_label_id(&end_label);
            for (cond, body) in pairs {
                let next_label = scope.new_label();
                let next_label_id = scope.get_label_id(&next_label);
                translate_expr(code, scope, cond)?;
                code.add(Opcode::GotoIfFalse(next_label_id), cond.mark.clone());
                translate_stmt(code, scope, body)?;
                code.add(Opcode::Goto(end_label_id), body.mark.clone());
                scope.update_label(&next_label, code.len());
            }
            if let Some(other) = other {
                translate_stmt(code, scope, other)?;
            }
            scope.update_label(&end_label, code.len());
        }
        StmtDesc::While(cond, body) => {
            let start_label = scope.new_label();
            let start_label_id = scope.get_label_id(&start_label);
            let end_label = scope.new_label();
            let end_label_id = scope.get_label_id(&end_label);
            scope.update_label(&start_label, code.len());
            translate_expr(code, scope, cond)?;
            code.add(Opcode::GotoIfFalse(end_label_id), cond.mark.clone());
            translate_stmt(code, scope, body)?;
            code.add(Opcode::Goto(start_label_id), stmt.mark.clone());
            scope.update_label(&end_label, code.len());
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
    }
    Ok(())
}

struct Scope {
    file_name: RcStr,
    globals: HashMap<RcStr, Item>,
    locals: Option<HashMap<RcStr, Item>>,
    labels: Vec<(Vec<RcStr>, HashMap<RcStr, usize>, Vec<usize>)>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            file_name: "".to_owned().into(),
            globals: HashMap::new(),
            locals: None,
            labels: vec![(vec![], HashMap::new(), vec![])],
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
            None => {
                println!("{:?}", self.globals);
                Err(BasicError {
                    marks: vec![mark.clone()],
                    message: format!("Variable {} not found", name),
                })
            }
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
    fn new_label_with_name(&mut self, name: RcStr) -> u32 {
        let (labels, map, ptrs) = self.labels.last_mut().unwrap();
        assert!(!map.contains_key(&name));
        let id = labels.len();
        labels.push(name.clone());
        map.insert(name, id);
        ptrs.push(INVALID_LABEL_LOC);
        id as u32
    }
    pub fn new_label(&mut self) -> RcStr {
        let name: RcStr = format!("#{}", self.labels.last().unwrap().0.len()).into();
        self.new_label_with_name(name.clone());
        name
    }
    pub fn get_label_id(&mut self, name: &RcStr) -> u32 {
        let (_labels, map, _ptrs) = self.labels.last().unwrap();
        if let Some(id) = map.get(name) {
            *id as u32
        } else {
            self.new_label_with_name(name.clone())
        }
    }
    pub fn update_label(&mut self, name: &RcStr, loc: usize) {
        let id = self.get_label_id(name) as usize;
        self.labels.last_mut().unwrap().2[id] = loc;
    }
    pub fn enter_local(&mut self) {
        if self.locals.is_some() {
            panic!("Scope::enter_local: already in local scope");
        }
        self.locals = Some(HashMap::new());
        self.labels.push((vec![], HashMap::new(), vec![]));
    }
    pub fn exit_local(&mut self) {
        if self.locals.is_none() {
            panic!("Scope::exit_local: not in local scope");
        }
        self.locals = None;
        self.labels.pop().unwrap();
    }
    pub fn label_map(&self) -> &Vec<usize> {
        &self.labels.last().unwrap().2
    }
    pub fn label_names(&self) -> &Vec<RcStr> {
        &self.labels.last().unwrap().0
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
