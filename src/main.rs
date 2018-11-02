#[macro_use]
extern crate lazy_static;
extern crate unicode_segmentation;

mod ast;
mod interpreter;
mod parser;
mod scanner;
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
use scanner::*;
use value::*;

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
                let result = run(&mut interpreter, input);
                print_result(&result, true);
            }
            Err(error) => {
                println!("Error reading stdin: {:?}", error);
                break;
            }
        }
    }
}

fn run_file(file_path: &str) -> bool {
    let mut file = File::open(file_path).expect(&format!("source file not found: {}", file_path));
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect(&format!("unable to read file: {}", file_path));

    let mut interpreter = Interpreter::new();
    let result = run(&mut interpreter, contents);
    print_result(&result, false);

    result.is_err()
}

fn run(interpreter: &mut Interpreter, source: String) -> Result<Value, RuntimeError> {
    let mut scanner = Scanner::new(&source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    let result = interpreter.interpret(ast);

    result
}

fn print_result(result: &Result<Value, RuntimeError>, print_success: bool) {
    match result {
        Ok(value) => {
            if print_success {
                println!("{}", value);
            }
        }
        Err(e) => {
            util::error(e.source_loc.line, &e.message);
        }
    }
}
