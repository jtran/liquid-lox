use token::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, TokenType, Box<Expr>),
    Grouping(Box<Expr>),
    LiteralBool(bool),
    LiteralNumber(f64),
    LiteralNil,
    LiteralString(String),
    Unary(TokenType, Box<Expr>),
}
