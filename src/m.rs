use super::translate_files;
use super::BasicError;
use super::DefaultHandler;
use super::Loader;
use super::Vm;
use super::RcStr;

pub fn main() {
    let mut loader = Loader::new();
    let mut module_name: Option<RcStr> = None;
    let mut state = State::Normal;

    for arg in std::env::args() {
        let arg: &str = &arg;

        match &state {
            State::Normal => match arg {
                "-m" => state = State::Module,
                _ => loader.add_source_root(arg),
            },
            State::Module => {
                module_name = Some(arg.to_owned().into());
                state = State::Normal;
            }
        }
    }

    match module_name {
        Some(module_name) => match run(&mut loader, &module_name) {
            Ok(()) => {}
            Err(error) => {
                eprintln!("{}", error.format());
                std::process::exit(1);
            }
        },
        None => {
            eprintln!("Module name not specified");
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
            marks: vec![],
            message: format!("{}", error),
        }),
    }
}

enum State {
    Normal,
    Module,
}
