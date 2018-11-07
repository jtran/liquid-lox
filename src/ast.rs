use source_loc::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Break(SourceLoc),
    Expression(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    Var(String, Expr),
    While(Expr, Box<Stmt>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign(String, Box<Expr>, SourceLoc),
    Call(Box<Expr>, Vec<Expr>, SourceLoc),
    Binary(Box<Expr>, BinaryOperator, Box<Expr>, SourceLoc),
    Grouping(Box<Expr>),
    LiteralBool(bool),
    LiteralNumber(f64),
    LiteralNil,
    LiteralString(String),
    Logical(Box<Expr>, LogicalOperator, Box<Expr>),
    Variable(String, SourceLoc),
    Unary(UnaryOperator, Box<Expr>, SourceLoc),
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
