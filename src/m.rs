use super::Handler;
use super::OtherHandler;
use super::DefaultHandler;

pub fn main() {
    let mut module_name: Option<String> = None;
    let mut state = State::Normal;
    let mut test_flag = false;
    let mut source_roots = Vec::new();
    let mut handler_type = HandlerType::Default;

    for argstring in std::env::args() {
        let arg: &str = &argstring;

        match &state {
            State::Normal => match arg {
                "-m" => state = State::Module,
                "-h" => state = State::Handler,
                "-t" => test_flag = true,
                _ => source_roots.push(argstring),
            },
            State::Module => {
                module_name = Some(arg.to_owned());
                state = State::Normal;
            }
            State::Handler => {
                handler_type = match arg {
                    "other" | "o" => HandlerType::Other,
                    "default" | "d" => HandlerType::Default,
                    _ => panic!("Unrecognized handler type: {:?}", arg),
                };
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
        match handler_type {
            HandlerType::Default => DefaultHandler.test(source_roots, module_name),
            HandlerType::Other => OtherHandler::new().test(source_roots, module_name),
        }
    } else {
        let run = match handler_type {
            HandlerType::Default => DefaultHandler::run,
            HandlerType::Other => OtherHandler::run,
        };
        run(source_roots, module_name);
    };
}

enum HandlerType {
    Default,
    Other,
}

enum State {
    Normal,
    Module,
    Handler,
}
