use super::translate_files;
use super::BasicError;
use super::Loader;
use super::RcStr;
use super::Scope;
use super::Val;
use super::Vm;

/// Interface to the outside world
pub trait Handler
where
    Self: Sized,
{
    fn run(source_roots: Vec<String>, module_name: String);

    fn test(_source_roots: Vec<String>, _module_name: String) {
        panic!(
            "Testing with {:?} is not supported",
            std::any::type_name::<Self>()
        )
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
        Self::run_with_handler(DefaultHandler, source_roots, module_name, false)
    }

    fn test(source_roots: Vec<String>, module_name: String) {
        Self::run_with_handler(DefaultHandler, source_roots, module_name, true)
    }

    fn print(&mut self, _: &mut Scope, val: Val) -> Result<(), Val> {
        println!("{}", val);
        Ok(())
    }
}

/// Some helper functions to make implementing Handlers more convenient
impl DefaultHandler {
    pub(crate) fn run_with_handler<H: Handler>(handler: H, source_roots: Vec<String>, module_name: String, test: bool) {
        match Self::run_with_handler0(handler, source_roots, module_name, test) {
            Ok(()) => {}
            Err(error) => {
                eprintln!("{}", error.format());
                std::process::exit(1);
            }
        }
    }
    pub(crate) fn run_with_handler0<H: Handler>(handler: H, source_roots: Vec<String>, module_name: String, test: bool) -> Result<(), BasicError> {
        let module_name: RcStr = module_name.into();
        let mut loader = Loader::new();
        for source_root in source_roots {
            loader.add_source_root(source_root);
        }

        let files = if test {
            loader.add_test_source(&module_name)?;
            loader.load(&"[test]".into())?
        } else {
            loader.load(&module_name)?
        };

        let mut vm = Vm::new(handler);

        let code = translate_files(files)?;

        let r = if test {
            println!("testing {}", module_name);
            vm.exec_and_run_tests(&code, &vec![
                format!("{}#", module_name).into(),
                format!("{}.", module_name).into(),
            ])
        } else {
            vm.exec(&code)
        };
        err_trace(&mut vm, r)
    }
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
