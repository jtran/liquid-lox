#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, BinaryOperator, Box<Expr>, SourceLoc),
    Grouping(Box<Expr>),
    LiteralBool(bool),
    LiteralNumber(f64),
    LiteralNil,
    LiteralString(String),
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
pub struct SourceLoc {
    pub line: usize,
}

impl SourceLoc {
    pub fn new(line: usize) -> SourceLoc {
        SourceLoc {
            line,
        }
    }
}
