use std::fmt;

use parser::ParseError;
use source_loc::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    BoolVal(bool),
    NilVal,
    NumberVal(f64),
    StringVal(String),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum RuntimeType {
    BoolType,
    NilType,
    NumberType,
    StringType,
}

use self::Value::*;

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            BoolVal(b) => *b,
            NilVal => false,
            NumberVal(_) | StringVal(_) => true,
        }
    }

    pub fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (BoolVal(b1), BoolVal(b2)) => b1 == b2,
            (NilVal, NilVal) => true,
            (NumberVal(x1), NumberVal(x2)) => x1 == x2,
            (StringVal(s1), StringVal(s2)) => s1 == s2,
            (_, _) => false,
        }
    }

    pub fn runtime_type(&self) -> RuntimeType {
        match self {
            BoolVal(_) => RuntimeType::BoolType,
            NilVal => RuntimeType::NilType,
            NumberVal(_) => RuntimeType::NumberType,
            StringVal(_) => RuntimeType::StringType,
        }
    }

    pub fn to_runtime_string(&self) -> String {
        match self {
            BoolVal(true) => "true".into(),
            BoolVal(false) => "false".into(),
            NilVal => "nil".into(),
            NumberVal(x) => format!("{}", x),
            StringVal(s) => s.clone(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BoolVal(false) => write!(f, "false"),
            BoolVal(true) => write!(f, "true"),
            NilVal => write!(f, "nil"),
            NumberVal(x) => write!(f, "{}", x),
            StringVal(s) => write!(f, "\"{}\"", s),
        }
    }
}

impl fmt::Display for RuntimeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::RuntimeType::*;
        match self {
            BoolType => write!(f, "bool"),
            NilType => write!(f, "nil"),
            NumberType => write!(f, "number"),
            StringType => write!(f, "string"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RuntimeError {
    pub source_loc: SourceLoc,
    pub message: String,
}

impl RuntimeError {
    pub fn new(source_loc: SourceLoc, message: &str) -> RuntimeError {
        RuntimeError {
            source_loc,
            message: message.into(),
        }
    }
}

impl From<ParseError> for RuntimeError {
    fn from(err: ParseError) -> RuntimeError {
        RuntimeError::new(err.source_loc(), &format!("parse error: {}", &err.message()))
    }
}