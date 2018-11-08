#[macro_use]
extern crate lazy_static;
extern crate unicode_segmentation;

mod ast;
mod environment;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod source_loc;
mod token;
mod util;
mod value;

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::process;

use interpreter::*;
use parser::*;
use value::*;

enum RunError {
    RunParseError(ParseError),
    RunRuntimeError(RuntimeError),
}

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() > 2 {
        print_usage();
        process::exit(1);
    }
    else if args.len() == 2 {
        let had_runtime_error = run_file(&args[1]);

        if had_runtime_error {
            process::exit(70);
        }
    }
    else {
        run_repl();
    }
}

fn print_usage() {
    println!("Usage: lox <source-file>");
}

fn run_repl() {
    let stdin = io::stdin();
    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        io::stdout().flush().expect("run_repl: unable to flush stdout");

        let mut input = String::new();
        match stdin.lock().read_line(&mut input) {
            Ok(_) => {
                let result = run(&mut interpreter, input, true);
                print_result(&result, true);
            }
            Err(error) => {
                println!("Error reading stdin: {:?}", error);
                break;
            }
        }
    }
}

// Returns true if there was an error running the file.
fn run_file(file_path: &str) -> bool {
    let mut file = File::open(file_path).expect(&format!("source file not found: {}", file_path));
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect(&format!("unable to read file: {}", file_path));

    let mut interpreter = Interpreter::new();
    let result = run(&mut interpreter, contents, false);
    print_result(&result, false);

    result.is_err()
}

fn run(interpreter: &mut Interpreter, source: String, for_repl: bool)
    -> Result<Value, RunError>
{
    // If there's a parse error, it's converted to a run error here.
    let mut ast = if for_repl {
        parser::parse_repl_line(&source)
    }
    else {
        parser::parse(&source)
    }?;
    resolver::resolve(&mut ast).map_err(|e| ParseError::from(e))?;
    let result = interpreter.interpret(ast);

    result.map_err(|err| err.into())
}

fn print_result(result: &Result<Value, RunError>, print_success: bool) {
    match result {
        Ok(value) => {
            if print_success {
                println!("{}", value);
            }
        }
        Err(e) => {
            match e {
                RunError::RunParseError(err) => {
                    // Print all causes.
                    for cause in err.causes.iter() {
                        util::error(cause.source_loc.line, &cause.message);
                    }
                }
                RunError::RunRuntimeError(err) => {
                    util::error(err.source_loc.line, &err.message);
                }
            }
        }
    }
}

impl From<ParseError> for RunError {
    fn from(err: ParseError) -> RunError {
        RunError::RunParseError(err)
    }
}

impl From<RuntimeError> for RunError {
    fn from(err: RuntimeError) -> RunError {
        RunError::RunRuntimeError(err)
    }
}
