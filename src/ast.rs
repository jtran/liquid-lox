use source_loc::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Expression(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    Var(String, Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign(String, Box<Expr>, SourceLoc),
    Binary(Box<Expr>, BinaryOperator, Box<Expr>, SourceLoc),
    Grouping(Box<Expr>),
    LiteralBool(bool),
    LiteralNumber(f64),
    LiteralNil,
    LiteralString(String),
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
