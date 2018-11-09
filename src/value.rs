use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use ast::*;
use environment::*;
use parser::ParseError;
use source_loc::*;
use util;

#[derive(Clone, PartialEq)]
pub enum Value {
    BoolVal(bool),
    ClosureVal(Rc<FunctionDefinition>, Rc<RefCell<Environment>>),
    NativeFunctionVal(NativeFunctionId),
    NilVal,
    NumberVal(f64),
    StringVal(String),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum RuntimeType {
    BoolType,
    CallableType,
    NilType,
    NumberType,
    StringType,
}

use self::Value::*;

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            BoolVal(b) => *b,
            ClosureVal(_, _) => true,
            NativeFunctionVal(_) => true,
            NilVal => false,
            NumberVal(_) | StringVal(_) => true,
        }
    }

    pub fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (BoolVal(b1), BoolVal(b2)) => b1 == b2,
            (ClosureVal(fun_def1, env1), ClosureVal(fun_def2, env2)) => {
                // Checking environments first since it's a fast pointer
                // equality.  It also kind of matters that they are the same
                // environments because they can be mutated.
                util::same_object::<Rc<_>>(env1, env2) && fun_def1 == fun_def2
            }
            (NativeFunctionVal(id1), NativeFunctionVal(id2)) => id1 == id2,
            (NilVal, NilVal) => true,
            (NumberVal(x1), NumberVal(x2)) => x1 == x2,
            (StringVal(s1), StringVal(s2)) => s1 == s2,
            (_, _) => false,
        }
    }

    pub fn runtime_type(&self) -> RuntimeType {
        match self {
            BoolVal(_) => RuntimeType::BoolType,
            ClosureVal(_, _) => RuntimeType::CallableType,
            NativeFunctionVal(_) => RuntimeType::CallableType,
            NilVal => RuntimeType::NilType,
            NumberVal(_) => RuntimeType::NumberType,
            StringVal(_) => RuntimeType::StringType,
        }
    }

    pub fn to_runtime_string(&self) -> String {
        match self {
            BoolVal(true) => "true".into(),
            BoolVal(false) => "false".into(),
            ClosureVal(fun_def, _) => format!("<fn {}>", fun_def.name),
            NativeFunctionVal(id) => format!("<native fn {}>", id),
            NilVal => "nil".into(),
            NumberVal(x) => format!("{}", x),
            StringVal(s) => s.clone(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // This is like inspect and tries to output the code you would need to
        // write to get the value.
        match self {
            BoolVal(false) => write!(f, "false"),
            BoolVal(true) => write!(f, "true"),
            ClosureVal(fun_def, _) => {
                let param_names = fun_def.parameters.iter()
                    .map(|p| p.name.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "fun {}({}) {{...}}", fun_def.name, param_names)
            }
            NativeFunctionVal(id) => write!(f, "{}", id),
            NilVal => write!(f, "nil"),
            NumberVal(x) => write!(f, "{}", x),
            StringVal(s) => write!(f, "\"{}\"", s),
        }
    }
}

// We can't derive this because the Rc recursively prints the values in the
// environment, causing a stack overflow.
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BoolVal(b) => write!(f, "BoolVal({:?})", b),
            ClosureVal(fun_def, _) => {
                write!(f, "ClosureVal({:?}, Rc(...))", fun_def)
            }
            NativeFunctionVal(id) => write!(f, "NativeFunctionVal({:?})", id),
            NilVal => write!(f, "NilVal"),
            NumberVal(x) => write!(f, "NumberVal({:?})", x),
            StringVal(s) => write!(f, "StringVal({:?})", s),
        }
    }
}

impl fmt::Display for RuntimeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::RuntimeType::*;
        match self {
            BoolType => write!(f, "bool"),
            CallableType => write!(f, "callable"),
            NilType => write!(f, "nil"),
            NumberType => write!(f, "number"),
            StringType => write!(f, "string"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExecutionInterrupt {
    Error(RuntimeError),
    Break(SourceLoc),
    Return(Value),
}

impl From<ParseError> for ExecutionInterrupt {
    fn from(error: ParseError) -> ExecutionInterrupt {
        ExecutionInterrupt::Error(error.into())
    }
}

impl From<RuntimeError> for ExecutionInterrupt {
    fn from(error: RuntimeError) -> ExecutionInterrupt {
        ExecutionInterrupt::Error(error)
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

impl From<ExecutionInterrupt> for RuntimeError {
    fn from(interrupt: ExecutionInterrupt) -> RuntimeError {
        match interrupt {
            // If you hit this error, it's probably due to a break statement
            // outside of a loop.  The parser should disallow this.
            ExecutionInterrupt::Break(_) => panic!("Unexpected break execution interrupt: {:?}", &interrupt),
            ExecutionInterrupt::Error(error) => error,
            ExecutionInterrupt::Return(_) => panic!("Unexpected return execution interrupt: {:?}", &interrupt),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NativeFunctionId {
    Clock,
}

impl fmt::Display for NativeFunctionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::NativeFunctionId::*;
        match self {
            Clock => write!(f, "clock"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

    #[test]
    fn test_size_of_value() {
        assert_eq!(mem::size_of::<Value>(), 32);
    }
}
