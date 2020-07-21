use super::ast::*;
use super::BasicError;
use super::Binop;
use super::Code;
use super::Opcode;
use super::VarScope;
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
        for var in &file.vars {
            scope.decl(Item::Var(var.clone()))?;
        }
    }

    let mut code = Code {
        name: format!("[main]").into(),
        vars: global_vars,
        nparams: 0,
        ops: vec![],
        marks: vec![],
    };

    // translate all statements inside functions and
    // initialize all functions at the global scope
    for file in &files {
        scope.file_name = file.name().clone();
        for func in &file.funcs {
            let func_code = translate_func(&mut scope, func)?;
            code.ops.push(Opcode::NewFunc(Rc::new(func_code)));
            let var = func.as_var.as_ref().unwrap();
            code.ops.push(Opcode::Set(var.vscope, var.index));
        }
    }

    // translate all other global statements
    for file in &files {
        scope.file_name = file.name().clone();
        translate_stmt(&mut code, &mut scope, &file.body)?;
    }

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
    prefix: Option<&Rc<String>>,
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
        StmtDesc::Expr(_) | StmtDesc::Return(_) => {}
    }
    Ok(())
}

fn mkvar(mark: Mark, name: &Rc<String>, file_name: Option<&Rc<String>>, index: usize) -> Var {
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
    let mut code = Code {
        name: func.full_name().clone(),
        vars: func.vars.clone(),
        nparams: func.params.len(),
        ops: vec![],
        marks: vec![],
    };
    scope.locals = Some(HashMap::new());
    for var in &func.vars {
        scope.decl(Item::Var(var.clone()))?;
    }
    translate_stmt(&mut code, scope, &func.body)?;
    scope.locals = None;
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
    file_name: Rc<String>,
    globals: HashMap<Rc<String>, Item>,
    locals: Option<HashMap<Rc<String>, Item>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            file_name: "".to_owned().into(),
            globals: HashMap::new(),
            locals: None,
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
            Ok(())
        }
    }
    pub fn getvar_or_error(&self, mark: &Mark, name: &String) -> Result<&Var, BasicError> {
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
    pub fn rget(&self, name: &String) -> Option<&Item> {
        self.qget(name)
            .or_else(|| self.qget(&format!("{}#{}", self.file_name, name)))
    }
    pub fn qget(&self, qualified_name: &String) -> Option<&Item> {
        self.locals
            .as_ref()
            .and_then(|locals| locals.get(qualified_name))
            .or_else(|| self.globals.get(qualified_name))
    }
}

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
    pub fn name(&self) -> &Rc<String> {
        match self {
            Self::Var(var) => &var.name,
            Self::Import(imp) => &imp.unique_name,
        }
    }
}
