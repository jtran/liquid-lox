use std::cell::RefCell;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

use crate::ast::*;
use crate::environment::*;
use crate::field_table::*;
use crate::parser::ParseError;
use crate::source_loc::*;

#[derive(Clone, PartialEq)]
pub enum Value {
    BoolVal(bool),
    ClassVal(Rc<RuntimeClass>),
    ClosureVal(Closure),
    InstanceVal(InstanceRef),
    NativeFunctionVal(NativeFunctionId),
    NilVal,
    NumberVal(f64),
    StringVal(Rc<String>),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum RuntimeType {
    BoolType,
    CallableType,
    ClassType,
    InstanceType,
    NilType,
    NumberType,
    StringType,
}

use self::Value::*;

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            BoolVal(b) => *b,
            ClassVal(_) => true,
            ClosureVal(_) => true,
            InstanceVal(_) => true,
            NativeFunctionVal(_) => true,
            NilVal => false,
            NumberVal(_) | StringVal(_) => true,
        }
    }

    pub fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (BoolVal(b1), BoolVal(b2)) => b1 == b2,
            (ClassVal(class1), ClassVal(class2)) => {
                Rc::ptr_eq(class1, class2)
            }
            (ClosureVal(Closure(fun_def1, env1)), ClosureVal(Closure(fun_def2, env2))) => {
                // Checking environments first since it's a fast pointer
                // equality.  It also kind of matters that they are the same
                // environments because they can be mutated.
                Rc::ptr_eq(env1, env2) && fun_def1 == fun_def2
            }
            (InstanceVal(instance1), InstanceVal(instance2)) => instance1 == instance2,
            (NativeFunctionVal(id1), NativeFunctionVal(id2)) => id1 == id2,
            (NilVal, NilVal) => true,
            (NumberVal(x1), NumberVal(x2)) => x1 == x2,
            (StringVal(s1), StringVal(s2)) => s1.deref() == s2.deref(),
            (_, _) => false,
        }
    }

    pub fn runtime_type(&self) -> RuntimeType {
        match self {
            BoolVal(_) => RuntimeType::BoolType,
            ClassVal(_) => RuntimeType::ClassType,
            ClosureVal(_) => RuntimeType::CallableType,
            InstanceVal(_) => RuntimeType::InstanceType,
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
            ClassVal(class_def) => format!("<class {}>", class_def.name),
            ClosureVal(Closure(fun_def, _)) => format!("<fn {}>", fun_def.name),
            InstanceVal(instance_ref) => format!("<instance {}>", instance_ref.class_name()),
            NativeFunctionVal(id) => format!("<native fn {}>", id),
            NilVal => "nil".into(),
            NumberVal(x) => format!("{}", x),
            StringVal(s) => s.deref().clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeClass {
    name: String,
    methods: FieldTable,
}

impl RuntimeClass {
    pub fn new(name: &str, methods: FieldTable) -> RuntimeClass {
        RuntimeClass {
            name: name.to_string(),
            methods,
        }
    }

    pub fn find_method(&self, name: &str) -> Option<Value> {
        self.methods.get(name)
    }
}

#[derive(Clone, Debug)]
pub struct InstanceRef(pub Rc<RefCell<Instance>>);

impl InstanceRef {
    pub fn new(class: Rc<RuntimeClass>) -> InstanceRef {
        let instance = Instance::new(class);

        InstanceRef(Rc::new(RefCell::new(instance)))
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.0.borrow().get(name, Rc::clone(&self.0))
    }

    pub fn set(&mut self, name: &str, new_value: Value) {
        self.0.deref().borrow_mut().set(name, new_value)
    }

    pub fn class_name(&self) -> String {
        self.0.borrow().class.name.to_string()
    }
}

impl PartialEq for InstanceRef {
    fn eq(&self, other: &InstanceRef) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    class: Rc<RuntimeClass>,
    fields: FieldTable,
}

impl Instance {
    pub fn new(class: Rc<RuntimeClass>) -> Instance {
        Instance {
            class,
            fields: FieldTable::new(),
        }
    }

    pub fn get(&self, name: &str, this_ref: Rc<RefCell<Instance>>) -> Option<Value> {
        let v = self.fields.get(name);
        if v.is_some() {
            return v;
        }

        match self.class.find_method(name) {
            None => (),
            Some(ClosureVal(cls)) => {
                let bound_method = cls.bind(Value::InstanceVal(InstanceRef(this_ref)));
                return Some(Value::ClosureVal(bound_method));
            }
            Some(v) => panic!("Accessing a property and looking up a method resulted in a non-closure value: v={:?}, name={}, instance={:?}", v, name, self),
        }

        None
    }

    pub fn set(&mut self, name: &str, new_value: Value) {
        self.fields.set(name, new_value);
    }
}

#[derive(Clone, PartialEq)]
pub struct Closure(pub Rc<FunctionDefinition>, pub Rc<RefCell<Environment>>);

impl Closure {
    pub fn arity(&self) -> usize {
        self.0.parameters.len()
    }

    pub fn bind(&self, this_value: Value) -> Closure {
        // Create a new environment.
        let mut new_env = Environment::new(Some(Rc::clone(&self.1)));
        new_env.define("this", this_value);

        Closure(Rc::clone(&self.0), Rc::new(RefCell::new(new_env)))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // This is like inspect and tries to output the code you would need to
        // write to get the value.
        match self {
            BoolVal(false) => write!(f, "false"),
            BoolVal(true) => write!(f, "true"),
            ClassVal(class_def) => {
                write!(f, "class {}{{...}}", class_def.name)
            }
            ClosureVal(Closure(fun_def, _)) => {
                let param_names = fun_def.parameters.iter()
                    .map(|p| p.name.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "fun {}({}) {{...}}", fun_def.name, param_names)
            }
            InstanceVal(instance_ref) => {
                write!(f, "{}()", instance_ref.class_name())
            }
            NativeFunctionVal(id) => write!(f, "{}", id),
            NilVal => write!(f, "nil"),
            NumberVal(x) => write!(f, "{}", x),
            StringVal(s) => write!(f, "\"{}\"", s.deref()),
        }
    }
}

// We can't derive this because the Rc recursively prints the values in the
// environment, causing a stack overflow.
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BoolVal(b) => write!(f, "BoolVal({:?})", b),
            ClassVal(class_def) => write!(f, "ClassVal({:?})", class_def),
            ClosureVal(Closure(fun_def, _)) => {
                write!(f, "ClosureVal(Closure({:?}, Rc(...)))", fun_def)
            }
            InstanceVal(instance) => write!(f, "InstanceVal({:?})", instance),
            NativeFunctionVal(id) => write!(f, "NativeFunctionVal({:?})", id),
            NilVal => write!(f, "NilVal"),
            NumberVal(x) => write!(f, "NumberVal({:?})", x),
            StringVal(s) => write!(f, "StringVal({:?})", s),
        }
    }
}

// We can't derive this because the Rc recursively prints the values in the
// environment, causing a stack overflow.
impl fmt::Debug for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Closure({:?}, Rc(...))", self.0)
    }
}

impl fmt::Display for RuntimeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::RuntimeType::*;
        match self {
            BoolType => write!(f, "bool"),
            ClassType => write!(f, "class"),
            CallableType => write!(f, "callable"),
            InstanceType => write!(f, "instance"),
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

pub fn all_native_ids() -> Vec<NativeFunctionId> {
    vec![NativeFunctionId::Clock]
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
        assert_eq!(mem::size_of::<Value>(), 24);
    }
}
