use super::translate_files;
use super::BasicError;
use super::DefaultHandler;
use super::Handler;
use super::Loader;
use super::RcStr;
use super::Source;
use super::Val;
use super::Vm;

pub fn main() {
    let mut loader = Loader::new();
    let mut module_name: Option<RcStr> = None;
    let mut state = State::Normal;
    let mut test_flag = false;

    for arg in std::env::args() {
        let arg: &str = &arg;

        match &state {
            State::Normal => match arg {
                "-m" => state = State::Module,
                "-t" => test_flag = true,
                _ => loader.add_source_root(arg),
            },
            State::Module => {
                module_name = Some(arg.to_owned().into());
                state = State::Normal;
            }
        }
    }

    let module_name = if let Some(module_name) = module_name {
        module_name
    } else {
        eprintln!("Start module name not specified");
        std::process::exit(1);
    };

    let result = if test_flag {
        run_tests(&mut loader, &module_name)
    } else {
        run(&mut loader, &module_name)
    };

    match result {
        Ok(()) => {}
        Err(error) => {
            eprintln!("{}", error.format());
            std::process::exit(1);
        }
    }
}

fn run(loader: &mut Loader, module_name: &RcStr) -> Result<(), BasicError> {
    let files = loader.load(&module_name)?;
    let code = translate_files(files)?;
    let mut vm = Vm::new(DefaultHandler);
    match vm.exec(&code) {
        Ok(_) => Ok(()),
        Err(error) => Err(BasicError {
            marks: vm.trace().clone(),
            message: format!("{}", error.as_err()),
            help: None,
        }),
    }
}

fn run_tests(loader: &mut Loader, module_name: &RcStr) -> Result<(), BasicError> {
    use std::fmt::Write;
    let child_modules = loader.list_child_modules(module_name)?;
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

enum State {
    Normal,
    Module,
}
