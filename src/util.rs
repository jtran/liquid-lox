use expr::Expr;
use parser::Parser;
use scanner::Scanner;

pub fn error(line: usize, message: &str) {
    println!("line {}: Error: {}", line, message);
}

pub fn parse(code: &str) -> Expr {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    ast
}
