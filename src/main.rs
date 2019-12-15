extern crate generational_arena;
#[macro_use]
extern crate lazy_static;
extern crate unicode_segmentation;

mod ast;
mod environment;
mod error;
mod field_table;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod source_loc;
mod token;
mod util;
mod value;

#[cfg(test)]
mod tests;

use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::process;

use argparse::{ArgumentParser, Print, Store};

use crate::error::*;
use crate::interpreter::*;
use crate::value::*;

enum RunError {
    RunParseError(ParseError),
    RunRuntimeError(RuntimeError),
}

fn main() {
    let mut script_filename = "".to_string();
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Lox language interpreter");
        ap.add_option(
            &["--version"],
            Print(env!("CARGO_PKG_VERSION").to_string()),
            "Show version",
        );
        ap.refer(&mut script_filename)
            .add_argument("script_filename", Store,
                          "Lox file to execute.  Omit to run an interactive REPL.");
        ap.parse_args_or_exit();
    }
    if ! script_filename.is_empty() {
        let run_result = run_file(&script_filename);

        match run_result {
            Ok(_) => (),
            Err(RunError::RunParseError(_)) => process::exit(65),
            Err(RunError::RunRuntimeError(_)) => process::exit(70),
        }
    }
    else {
        run_repl();
    }
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
fn run_file(file_path: &str) -> Result<Value, RunError> {
    let mut file = File::open(file_path).unwrap_or_else(|_| panic!("source file not found: {}", file_path));
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap_or_else(|_| panic!("unable to read file: {}", file_path));

    let mut interpreter = Interpreter::new();
    let result = run(&mut interpreter, contents, false);
    print_result(&result, false);

    result
}

fn run(interpreter: &mut Interpreter, source: String, for_repl: bool)
    -> Result<Value, RunError>
{
    // If there's a parse error, it's converted to a run error here.
    let ast = if for_repl {
        parser::parse_repl_line(&source)
    }
    else {
        parser::parse(&source)
    }?;
    let code = resolver::resolve(ast).map_err(|e| ParseError::from(e))?;
    let result = interpreter.interpret(&code);

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
                        print_parse_error(&cause);
                    }
                }
                RunError::RunRuntimeError(err) => {
                    print_runtime_error(&err);
                }
            }
        }
    }
}

fn print_parse_error(error: &ParseErrorCause) {
    match error.token {
        None => util::error(&error.source_loc, &error.message),
        Some(ref token) => util::error_at_token(&error.source_loc, &token, &error.message)
    }
}

fn print_runtime_error(error: &RuntimeError) {
    eprintln!("{}", error.message);
    // Print the backtrace.
    let mut loc = error.source_loc;
    for item in error.backtrace.iter() {
        eprintln!("[line {}:{}] in {}()", loc.line, loc.column, item.function_name);
        loc = item.source_loc;
    }
    eprintln!("[line {}:{}] in script", loc.line, loc.column);
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
