use super::translate_files;
use super::BasicError;
use super::Loader;
use super::RcStr;
use super::Scope;
use super::Val;
use super::Vm;
use super::Source;

/// Interface to the outside world
pub trait Handler where Self: Sized {
    fn run(source_roots: Vec<String>, module_name: String);

    fn test(self, source_roots: Vec<String>, module_name: String) {
        match run_test(source_roots, module_name) {
            Ok(()) => {}
            Err(error) => {
                eprintln!("{}", error.format());
                std::process::exit(1);
            }
        }
    }

    /// Behavior on 'print' statements
    fn print(&mut self, _: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }

    /// Returns seconds since UNIX epoch as float
    fn time(&mut self) -> f64 {
        std::time::UNIX_EPOCH.elapsed().unwrap().as_secs_f64()
    }
}

pub struct DefaultHandler;

impl Handler for DefaultHandler {
    fn run(source_roots: Vec<String>, module_name: String) {
        match run(Self, source_roots, module_name) {
            Ok(()) => {}
            Err(error) => {
                eprintln!("{}", error.format());
                std::process::exit(1);
            }
        }
    }

    fn print(&mut self, _: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }
}

fn run<H: Handler>(
    handler: H,
    source_roots: Vec<String>,
    module_name: String,
) -> Result<(), BasicError> {
    let module_name: RcStr = module_name.into();
    let mut loader = Loader::new();
    for source_root in source_roots {
        loader.add_source_root(source_root);
    }
    let files = loader.load(&module_name)?;
    let code = translate_files(files)?;
    let mut vm = Vm::new(handler);
    match vm.exec(&code) {
        Ok(_) => Ok(()),
        Err(error) => Err(BasicError {
            marks: vm.trace().clone(),
            message: format!("{}", error.as_err()),
            help: None,
        }),
    }
}

fn run_test(source_roots: Vec<String>, module_name: String) -> Result<(), BasicError> {
    use std::fmt::Write;
    let module_name: RcStr = module_name.into();
    let mut loader = Loader::new();
    for source_root in source_roots {
        loader.add_source_root(source_root);
    }
    let child_modules = loader.list_child_modules(&module_name)?;
    let mut test_src = String::new();
    let test_out = &mut test_src;
    let mut test_prefixes = Vec::new();

    for child_module in child_modules {
        writeln!(test_out, "import {}", child_module).unwrap();
        test_prefixes.push(format!("{}#", child_module).into());
    }

    loader.add_source(
        Source {
            name: "[test]".into(),
            data: test_src.into(),
        }
        .into(),
    );

    let files = loader.load(&"[test]".into())?;
    let code = translate_files(files)?;
    let mut vm = Vm::new(DefaultHandler);

    println!("testing {}", module_name);
    let r = vm.exec_and_run_tests(&code, &test_prefixes);
    err_trace(&mut vm, r)
}

fn err_trace<H: Handler, T>(vm: &mut Vm<H>, r: Result<T, Val>) -> Result<T, BasicError> {
    match r {
        Ok(t) => Ok(t),
        Err(error) => Err(BasicError {
            marks: vm.trace().clone(),
            message: format!("{}", error.as_err()),
            help: None,
        }),
    }
}
