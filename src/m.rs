use super::DefaultHandler;
use super::Handler;

pub fn main() {
    let mut module_name: Option<String> = None;
    let mut state = State::Normal;
    let mut test_flag = false;
    let mut source_roots = Vec::new();

    for argstring in std::env::args() {
        let arg: &str = &argstring;

        match &state {
            State::Normal => match arg {
                "-m" => state = State::Module,
                "-t" => test_flag = true,
                _ => source_roots.push(argstring),
            },
            State::Module => {
                module_name = Some(arg.to_owned());
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

    if test_flag {
        DefaultHandler::test(source_roots, module_name)
    } else {
        DefaultHandler::run(source_roots, module_name);
    };
}

enum State {
    Normal,
    Module,
}
