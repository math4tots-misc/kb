//! Handler implementation that binds lots of other stuff like SDL
//! This way, it's easier to separate out dependencies in the future if needed
use crate::Handler;
use crate::Scope;
use crate::Val;
use crate::RcStr;
use crate::Loader;
use crate::translate_files;
use crate::Vm;
use crate::BasicError;

mod conv;

// const START_WINDOW_WIDTH: u32 = 800;
// const START_WINDOW_HEIGHT: u32 = 600;

pub struct OtherHandler {}

impl OtherHandler {
    pub fn new() -> Self {
        Self {}
    }
}

impl Handler for OtherHandler {
    fn run(source_roots: Vec<String>, module_name: String) {
        let _handle = std::thread::Builder::new().name("kb-main".to_owned()).spawn(|| {
            let handler = OtherHandler::new();
            match run(handler, source_roots, module_name) {
                Ok(()) => {}
                Err(error) => {
                    eprintln!("{}", error.format());
                    std::process::exit(1);
                }
            }
        }).unwrap();
        _handle.join().unwrap();
    }

    fn print(&mut self, _scope: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }
}

// fn stoerr<T, S: Into<RcStr>>(r: Result<T, S>) -> Result<T, Val> {
//     match r {
//         Ok(t) => Ok(t),
//         Err(s) => Err(rterr(s.into())),
//     }
// }

// fn etoerr<T, E: std::error::Error>(r: Result<T, E>) -> Result<T, Val> {
//     match r {
//         Ok(t) => Ok(t),
//         Err(e) => Err(rterr(format!("{:?}", e))),
//     }
// }


fn run(handler: OtherHandler, source_roots: Vec<String>, module_name: String) -> Result<(), BasicError> {
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