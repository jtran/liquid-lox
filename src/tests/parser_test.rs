use std::cell::Cell;

use crate::ast::*;
use crate::ast::Expr::*;
use crate::environment::*;
use crate::error::*;
use crate::parser::*;
use crate::source_loc::*;

#[test]
fn test_parse_literal() {
    assert_eq!(parse_expression("42"), Ok(LiteralNumber(42.0)));
    assert_eq!(parse_expression("\"hello\""), Ok(LiteralString("hello".to_string())));
    assert_eq!(parse_expression("true"), Ok(LiteralBool(true)));
    assert_eq!(parse_expression("false"), Ok(LiteralBool(false)));
    assert_eq!(parse_expression("nil"), Ok(LiteralNil));
}

#[test]
fn test_parse_binary_op() {
    assert_eq!(parse_expression("40 + 2"), Ok(Binary(Box::new(LiteralNumber(40.0)),
                                                     BinaryOperator::Plus,
                                                     Box::new(LiteralNumber(2.0)),
                                                     SourceLoc::new(1, 4))));
}

#[test]
fn test_parse_unary_op() {
    assert_eq!(parse_expression("-42"), Ok(Unary(UnaryOperator::Minus,
                                                 Box::new(LiteralNumber(42.0)),
                                                 SourceLoc::new(1, 1))));
    assert_eq!(parse_expression("!true"), Ok(Unary(UnaryOperator::Not,
                                                   Box::new(LiteralBool(true)),
                                                   SourceLoc::new(1, 1))));
}

#[test]
fn test_parse_grouping() {
    assert_eq!(parse_expression("(40)"), Ok(Grouping(Box::new(LiteralNumber(40.0)))));
}

#[test]
fn test_parse_assign() {
    assert_eq!(parse_expression("x = 1"), Ok(Assign("x".to_string(),
                                                    Cell::new(VarLoc::placeholder()),
                                                    Box::new(LiteralNumber(1.0)),
                                                    SourceLoc::new(1, 3))));
}

#[test]
fn test_parse_super_property() {
    assert_eq!(parse_expression("super.x"), Ok(Super(Cell::new(VarLoc::placeholder()),
                                                     "x".to_string(),
                                                     SourceLoc::new(1, 1))));
    assert_eq!(parse("super.y;"), Ok(vec![Stmt::Expression(
                                              Super(Cell::new(VarLoc::placeholder()),
                                                    "y".to_string(),
                                                    SourceLoc::new(1, 1)))]));
}

#[test]
fn test_parse_comparison() {
    let ast = parse_expression("42 == 40 + 2");
    assert_eq!(ast, Ok(Binary(Box::new(LiteralNumber(42.0)),
                              BinaryOperator::Equal,
                              Box::new(Binary(Box::new(LiteralNumber(40.0)),
                                              BinaryOperator::Plus,
                                              Box::new(LiteralNumber(2.0)),
                                              SourceLoc::new(1, 10))),
                              SourceLoc::new(1, 4))));
}

#[test]
fn test_parse_invalid() {
    let causes = vec![
        ParseErrorCause::new_with_location(SourceLoc::new(1, 1), "and", "Expect expression."),
    ];
    assert_eq!(parse("and;"), Err(ParseError::new(causes)));
}

#[test]
fn test_parse_statements() {
    assert_eq!(parse("print \"one\";"), Ok(vec![Stmt::Print(LiteralString("one".into()))]));
}

#[test]
fn test_parse_while_loop_break() {
    let loc = SourceLoc::new(1, 14);
    assert_eq!(parse("while (true) break;"), Ok(vec![Stmt::While(LiteralBool(true),
                                                                 Box::new(Stmt::Break(loc)),
                                                                 None)]));
    assert!(parse("for (;;) if (true) break;").is_ok());
    assert!(parse("while (true) if (true) break;").is_ok());
    assert!(parse("while (true) while(true) break;").is_ok());

    assert!(parse("break;").is_err());
    assert!(parse("for (;;) nil; break;").is_err());
}
