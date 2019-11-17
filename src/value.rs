use std::cell::RefCell;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

use crate::ast::*;
use crate::environment::*;
use crate::error::ParseError;
use crate::field_table::*;
use crate::source_loc::*;

#[derive(Clone, PartialEq)]
pub enum Value {
    BoolVal(bool),
    ClassVal(ClassRef),
    ClosureVal(ClosureRef),
    InstanceVal(InstanceRef),
    NativeFunctionVal(NativeFunctionId),
    NilVal,
    NumberVal(f64),
    StringVal(Rc<String>),
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
            (ClassVal(class1), ClassVal(class2)) => class1 == class2,
            // It matters that they are the same environments because they can
            // be mutated.
            (ClosureVal(closure1), ClosureVal(closure2)) => closure1 == closure2,
            (InstanceVal(instance1), InstanceVal(instance2)) => instance1 == instance2,
            (NativeFunctionVal(id1), NativeFunctionVal(id2)) => id1 == id2,
            (NilVal, NilVal) => true,
            (NumberVal(x1), NumberVal(x2)) => x1 == x2,
            (StringVal(s1), StringVal(s2)) => s1.deref() == s2.deref(),
            (_, _) => false,
        }
    }

    pub fn to_runtime_string(&self) -> String {
        match self {
            BoolVal(true) => "true".into(),
            BoolVal(false) => "false".into(),
            ClassVal(class_ref) => class_ref.name().to_string(),
            ClosureVal(closure) => format!("<fn {}>", closure.name()),
            InstanceVal(instance_ref) => format!("{} instance", instance_ref.class_name()),
            NativeFunctionVal(_) => "<native fn>".to_string(),
            NilVal => "nil".into(),
            NumberVal(x) => {
                if *x == 0.0 && x.is_sign_negative() {
                    // Preserve the negative sign for negative zero.
                    format!("-{}", x.abs())
                } else {
                    format!("{}", x)
                }
            }
            StringVal(s) => s.deref().clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ClassRef(pub Rc<RefCell<RuntimeClass>>);

impl ClassRef {
    pub fn new(name: &str,
               superclass: Option<ClassRef>,
               fields: FieldTable,
               methods: FieldTable) -> ClassRef {
        let rt_class = RuntimeClass::new(name, superclass, fields, methods);

        ClassRef(Rc::new(RefCell::new(rt_class)))
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.0.borrow().get(name, self.clone())
    }

    pub fn set(&mut self, name: &str, new_value: Value) {
        self.0.deref().borrow_mut().set(name, new_value)
    }

    pub fn find_method(&self, name: &str) -> Option<Value> {
        self.0.borrow().find_method(name)
    }

    pub fn bound_method(&self, name: &str, instance_ref: InstanceRef) -> Option<Value> {
        self.0.borrow().bound_method(name, instance_ref)
    }

    pub fn find_class_method(&self, name: &str) -> Option<Value> {
        self.0.borrow().find_class_method(name)
    }

    pub fn bound_class_method(&self, name: &str, class_ref: ClassRef) -> Option<Value> {
        self.0.borrow().bound_class_method(name, class_ref)
    }

    pub fn name(&self) -> String {
        self.0.borrow().name.clone()
    }
}

impl PartialEq for ClassRef {
    fn eq(&self, other: &ClassRef) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeClass {
    name: String,
    superclass: Option<ClassRef>,
    fields: FieldTable,
    methods: FieldTable,
}

impl RuntimeClass {
    pub fn new(name: &str,
               superclass: Option<ClassRef>,
               fields: FieldTable,
               methods: FieldTable) -> RuntimeClass {
        RuntimeClass {
            name: name.to_string(),
            superclass,
            fields,
            methods,
        }
    }

    pub fn get(&self, name: &str, this_ref: ClassRef) -> Option<Value> {
        self.bound_class_method(name, this_ref)
    }

    pub fn set(&mut self, name: &str, new_value: Value) {
        self.fields.set(name, new_value);
    }

    pub fn find_method(&self, name: &str) -> Option<Value> {
        let v = self.methods.get(name);
        if v.is_some() {
            return v;
        }

        match &self.superclass {
            None => None,
            Some(superclass) => superclass.find_method(name),
        }
    }

    pub fn bound_method(&self, name: &str, instance_ref: InstanceRef) -> Option<Value> {
        match self.find_method(name) {
            None => None,
            Some(ClosureVal(cls)) => {
                let bound_method = cls.bind(Value::InstanceVal(instance_ref));

                Some(Value::ClosureVal(bound_method))
            }
            Some(v) => panic!("Accessing a property and looking up a method resulted in a non-closure value: name={}, class={:?}, v={:?}", name, self, v),
        }
    }

    pub fn find_class_method(&self, name: &str) -> Option<Value> {
        let v = self.fields.get(name);
        if v.is_some() {
            return v;
        }

        match &self.superclass {
            None => None,
            Some(superclass) => superclass.find_class_method(name),
        }
    }

    pub fn bound_class_method(&self, name: &str, class_ref: ClassRef) -> Option<Value> {
        match self.find_class_method(name) {
            None => None,
            Some(ClosureVal(cls)) => {
                let bound_method = cls.bind(Value::ClassVal(class_ref));

                Some(Value::ClosureVal(bound_method))
            }
            // This isn't a method, but some other value set on the class.  We
            // simply don't bind it.
            Some(v) => Some(v),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InstanceRef(pub Rc<RefCell<Instance>>);

impl InstanceRef {
    pub fn new(class: ClassRef) -> InstanceRef {
        let instance = Instance::new(class);

        InstanceRef(Rc::new(RefCell::new(instance)))
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.0.borrow().get(name, self.clone())
    }

    pub fn set(&mut self, name: &str, new_value: Value) {
        self.0.deref().borrow_mut().set(name, new_value)
    }

    pub fn class_name(&self) -> String {
        self.0.borrow().class.name()
    }
}

impl PartialEq for InstanceRef {
    fn eq(&self, other: &InstanceRef) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    class: ClassRef,
    fields: FieldTable,
}

impl Instance {
    pub fn new(class: ClassRef) -> Instance {
        Instance {
            class,
            fields: FieldTable::new(),
        }
    }

    pub fn get(&self, name: &str, this_ref: InstanceRef) -> Option<Value> {
        let v = self.fields.get(name);
        if v.is_some() {
            return v;
        }

        self.class.bound_method(name, this_ref)
    }

    pub fn set(&mut self, name: &str, new_value: Value) {
        self.fields.set(name, new_value);
    }
}

#[derive(Clone, Debug)]
pub struct ClosureRef(Rc<Closure>);

impl ClosureRef {
    pub fn new(fun_def: Rc<FunctionDefinition>,
               env: Rc<RefCell<Environment>>) -> ClosureRef {
        let closure = Closure(fun_def, env);

        ClosureRef(Rc::new(closure))
    }

    pub fn name(&self) -> String {
        self.0.name()
    }

    pub fn arity(&self) -> usize {
        self.0.arity()
    }

    pub fn parameters(&self) -> &[Parameter] {
        self.0.parameters()
    }

    pub fn function_definition(&self) -> &FunctionDefinition {
        &(self.0).0
    }

    pub fn env(&self) -> &Rc<RefCell<Environment>> {
        &(self.0).1
    }

    pub fn bind(&self, this_value: Value) -> ClosureRef {
        let raw_closure = self.0.bind(this_value);

        ClosureRef::new(raw_closure.0, raw_closure.1)
    }
}

impl PartialEq for ClosureRef {
    fn eq(&self, other: &ClosureRef) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, PartialEq)]
pub struct Closure(Rc<FunctionDefinition>, Rc<RefCell<Environment>>);

impl Closure {
    pub fn name(&self) -> String {
        self.0.name.clone()
    }

    pub fn arity(&self) -> usize {
        self.0.parameters.len()
    }

    pub fn parameters(&self) -> &[Parameter] {
        &self.0.parameters
    }

    pub fn bind(&self, this_value: Value) -> Closure {
        // Create a new environment.
        let mut new_env = Environment::new_with_parent(Rc::clone(&self.1));
        let this_frame_index = new_env.next_frame_index();
        new_env.define_at("this", this_frame_index, this_value);

        Closure(Rc::clone(&self.0), Rc::new(RefCell::new(new_env)))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // This is like inspect and tries to output the code you would need to
        // write to get the value.  This is used in the REPL.
        match self {
            BoolVal(false) => write!(f, "false"),
            BoolVal(true) => write!(f, "true"),
            ClassVal(class_ref) => {
                write!(f, "class {}{{...}}", class_ref.name())
            }
            ClosureVal(closure) => {
                let param_names = closure.parameters().iter()
                    .map(|p| p.name.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "fun {}({}) {{...}}", closure.name(), param_names)
            }
            InstanceVal(instance_ref) => {
                write!(f, "{}()", instance_ref.class_name())
            }
            NativeFunctionVal(id) => write!(f, "{}", id),
            NilVal => write!(f, "nil"),
            NumberVal(x) => {
                if *x == 0.0 && x.is_sign_negative() {
                    // The reference implementation prints negative zero.
                    write!(f, "-{}", x.abs())
                } else {
                    write!(f, "{}", x)
                }
            }
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
            ClosureVal(closure) => write!(f, "ClosureVal({:?})", closure),
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

#[derive(Clone, Debug, PartialEq)]
pub enum ExecutionInterrupt {
    Error(RuntimeError),
    Return(Value),
    Break(SourceLoc),
    Continue(SourceLoc),
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
            message: message.to_string(),
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
            ExecutionInterrupt::Continue(_) => panic!("Unexpected continue execution interrupt: {:?}", &interrupt),
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
        assert_eq!(mem::size_of::<Value>(), 16);
    }
}
