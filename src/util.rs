use ast::{Expr, Stmt};
use parser::*;
use scanner::Scanner;

pub fn error(line: usize, message: &str) {
    println!("line {}: Error: {}", line, message);
}

#[allow(dead_code)]
pub fn parse(code: &str) -> Result<Vec<Stmt>, ParseError> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    ast
}

#[allow(dead_code)]
pub fn parse_expression(code: &str) -> Result<Expr, ParseError> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_expression();

    ast
}
