#[macro_use]
extern crate lazy_static;
extern crate unicode_segmentation;

mod expr;
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

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() > 2 {
        print_usage();
        process::exit(1);
    }
    else if args.len() == 2 {
        run_file(&args[1]);
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
    let mut read = stdin.lock();
    loop {
        print!("> ");
        io::stdout().flush().expect("run_repl: unable to flush stdout");

        let mut input = String::new();
        match read.read_line(&mut input) {
            Ok(_) => {
                run(input);
            }
            Err(error) => {
                println!("Error reading stdin: {:?}", error);
                break;
            }
        }
    }
}

fn run_file(file_path: &str) {
    let mut file = File::open(file_path).expect(&format!("source file not found: {}", file_path));
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect(&format!("unable to read file: {}", file_path));

    run(contents)
}

fn run(source: String) {
    let mut scanner = Scanner::new(&source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    let mut interpreter = Interpreter::new();
    let result = interpreter.evaluate(&ast);
    println!("{:?}", result);
}
