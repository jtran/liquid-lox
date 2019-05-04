use std::cell::Cell;

use crate::source_loc::*;
use crate::environment::VarLoc;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Break(SourceLoc),
    Class(ClassDefinition),
    Expression(Expr),
    Fun(FunctionDefinition),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Print(Expr),
    Return(Expr, SourceLoc),
    Var(String, Expr),
    While(Expr, Box<Stmt>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign(String, Cell<VarLoc>, Box<Expr>, SourceLoc),
    Call(Box<Expr>, Vec<Expr>, SourceLoc),
    Binary(Box<Expr>, BinaryOperator, Box<Expr>, SourceLoc),
    Get(Box<Expr>, String, SourceLoc),
    Grouping(Box<Expr>),
    LiteralBool(bool),
    LiteralNumber(f64),
    LiteralNil,
    LiteralString(String),
    Logical(Box<Expr>, LogicalOperator, Box<Expr>),
    Set(Box<Expr>, String, Box<Expr>, SourceLoc),
    Super(Cell<VarLoc>, String, SourceLoc),
    Variable(String, Cell<VarLoc>, SourceLoc),
    Unary(UnaryOperator, Box<Expr>, SourceLoc),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassDefinition {
    pub name: String,
    pub superclass: Option<Expr>,
    pub methods: Vec<FunctionDefinition>,
    pub source_loc: SourceLoc,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Vec<Stmt>,
    pub fun_type: FunctionType,
    pub source_loc: SourceLoc,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FunctionType {
    PlainFunction,
    Method,
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

impl FunctionDefinition {
    pub fn new(name: String,
               parameters: Vec<Parameter>,
               body: Vec<Stmt>,
               fun_type: FunctionType,
               source_loc: SourceLoc)
        -> FunctionDefinition
    {
        FunctionDefinition {
            name,
            parameters,
            body,
            fun_type,
            source_loc,
        }
    }
}

impl Parameter {
    pub fn new(name: String) -> Parameter {
        Parameter {
            name,
        }
    }
}
