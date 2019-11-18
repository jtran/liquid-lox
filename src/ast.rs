use std::cell::Cell;

use crate::environment::{FrameIndex, VarLoc};
use crate::source_loc::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Break(SourceLoc),
    Class(ClassDefinition),
    Continue(SourceLoc),
    Expression(Expr),
    Fun(NamedFunctionDefinition),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    Return(Expr, SourceLoc),
    Var(String, Cell<FrameIndex>, Expr, SourceLoc),
    While(Expr, Box<Stmt>),
    WhileIncrement(Expr, Box<Stmt>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign(String, Cell<VarLoc>, Box<Expr>, SourceLoc),
    Call(Box<Expr>, Vec<Expr>, SourceLoc),
    Binary(Box<Expr>, BinaryOperator, Box<Expr>, SourceLoc),
    Function(Box<FunctionDefinition>),
    Get(Box<Expr>, String, SourceLoc),
    GetIndex(Box<Expr>, Box<Expr>, SourceLoc),
    Grouping(Box<Expr>),
    LiteralArray(Vec<Expr>),
    LiteralBool(bool),
    LiteralNumber(f64),
    LiteralNil,
    LiteralString(String),
    Logical(Box<Expr>, LogicalOperator, Box<Expr>),
    Set(Box<Expr>, String, Box<Expr>, SourceLoc),
    SetIndex(Box<Expr>, Box<Expr>, Box<Expr>, SourceLoc),
    Super(Cell<VarLoc>, String, SourceLoc),
    Variable(String, Cell<VarLoc>, SourceLoc),
    Unary(UnaryOperator, Box<Expr>, SourceLoc),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassDefinition {
    pub name: String,
    pub superclass: Option<Box<Expr>>,
    pub methods: Vec<NamedFunctionDefinition>,
    pub source_loc: SourceLoc,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedFunctionDefinition {
    pub name: String,
    pub fun_def: FunctionDefinition,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    pub parameters: Vec<Parameter>,
    pub body: Vec<Stmt>,
    pub fun_type: FunctionType,
    pub source_loc: SourceLoc,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub source_loc: SourceLoc,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FunctionType {
    PlainFunction,
    Method,
    Initializer,
    ClassMethod,
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

impl ClassDefinition {
    pub fn has_superclass(&self) -> bool {
        self.superclass.is_some()
    }
}

impl NamedFunctionDefinition {
    pub fn new(name: String, fun_def: FunctionDefinition) -> NamedFunctionDefinition {
        NamedFunctionDefinition {
            name,
            fun_def,
        }
    }
}

impl FunctionDefinition {
    pub fn new(parameters: Vec<Parameter>,
               body: Vec<Stmt>,
               fun_type: FunctionType,
               source_loc: SourceLoc)
        -> FunctionDefinition
    {
        FunctionDefinition {
            parameters,
            body,
            fun_type,
            source_loc,
        }
    }
}

impl Parameter {
    pub fn new(name: String, source_loc: SourceLoc) -> Parameter {
        Parameter {
            name,
            source_loc,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

    #[test]
    fn test_size_of_stmt() {
        assert_eq!(mem::size_of::<Stmt>(), 96);
    }

    #[test]
    fn test_size_of_class_definition() {
        assert_eq!(mem::size_of::<ClassDefinition>(), 64);
    }

    #[test]
    fn test_size_of_function_definition() {
        assert_eq!(mem::size_of::<FunctionDefinition>(), 64);
    }

    #[test]
    fn test_size_of_expr() {
        assert_eq!(mem::size_of::<Expr>(), 56);
    }
}
