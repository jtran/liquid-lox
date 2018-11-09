use std::cell::Cell;

use source_loc::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Break(SourceLoc),
    Expression(Expr),
    Fun(String, Vec<Parameter>, Vec<Stmt>, SourceLoc),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    Return(Expr, SourceLoc),
    Var(String, Expr),
    While(Expr, Box<Stmt>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign(String, Cell<usize>, Box<Expr>, SourceLoc),
    Call(Box<Expr>, Vec<Expr>, SourceLoc),
    Binary(Box<Expr>, BinaryOperator, Box<Expr>, SourceLoc),
    Grouping(Box<Expr>),
    LiteralBool(bool),
    LiteralNumber(f64),
    LiteralNil,
    LiteralString(String),
    Logical(Box<Expr>, LogicalOperator, Box<Expr>),
    Variable(String, Cell<usize>, SourceLoc),
    Unary(UnaryOperator, Box<Expr>, SourceLoc),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Not,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,

    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
}

impl AsRef<Stmt> for Stmt {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl Parameter {
    pub fn new(name: String) -> Parameter {
        Parameter {
            name,
        }
    }
}
