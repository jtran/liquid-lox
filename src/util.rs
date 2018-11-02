use ast::{Expr, Stmt};
use parser::Parser;
use scanner::Scanner;

pub fn error(line: usize, message: &str) {
    println!("line {}: Error: {}", line, message);
}

#[allow(dead_code)]
pub fn parse(code: &str) -> Vec<Stmt> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    ast
}

#[allow(dead_code)]
pub fn parse_expression(code: &str) -> Expr {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_expression();

    ast
}
