use std::cell::RefCell;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;
use std::slice::Iter;

use crate::ast::*;
use crate::environment::*;
use crate::error::ParseError;
use crate::field_table::*;
use crate::source_loc::*;

#[derive(Clone, PartialEq)]
pub enum Value {
    ArrayVal(Rc<RefCell<Vec<Value>>>),
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
            ArrayVal(_) => true,
            BoolVal(b) => *b,
            ClassVal(_) => true,
            ClosureVal(_) => true,
            InstanceVal(_) => true,
            NativeFunctionVal(_) => true,
            NilVal => false,
            NumberVal(_) | StringVal(_) => true,
        }
    }

    pub fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }

    pub fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (ArrayVal(a1), ArrayVal(a2)) => a1 == a2,
            (BoolVal(b1), BoolVal(b2)) => b1 == b2,
            (ClassVal(class1), ClassVal(class2)) => class1 == class2,
            // It matters that they are the same environments because they can
            // be mutated.
            (ClosureVal(closure1), ClosureVal(closure2)) => closure1 == closure2,
            (InstanceVal(instance1), InstanceVal(instance2)) => instance1 == instance2,
            (NativeFunctionVal(id1), NativeFunctionVal(id2)) => id1 == id2,
            (NilVal, NilVal) => true,
            (NumberVal(x1), NumberVal(x2)) => x1 == x2,
            (StringVal(s1), StringVal(s2)) => **s1 == **s2,
            (_, _) => false,
        }
    }

    // Equality optimized for when you have interned strings.
    pub fn is_equal_interned(&self, other: &Value) -> bool {
        match (self, other) {
            (StringVal(s1), StringVal(s2)) => Rc::ptr_eq(s1, s2),
            _ => self.is_equal(other),
        }
    }

    pub fn to_runtime_string(&self) -> String {
        match self {
            ArrayVal(vec) => format!("[{}]",
                                     vec.borrow().iter().map(|v| v.to_runtime_string())
                                        .collect::<Vec<String>>()
                                        .join(", ")),
            BoolVal(true) => "true".into(),
            BoolVal(false) => "false".into(),
            ClassVal(class_ref) => class_ref.name(),
            ClosureVal(closure) => {
                match closure.name() {
                    Some(name) => format!("<fn {}>", name),
                    None => "<fn>".to_string(),
                }
            }
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
        let v = self.fields.get(name);
        if v.is_some() {
            return v;
        }

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
            Some(v) => panic!("Accessing a property and looking up a method resulted in a non-closure value: name={}, class={:?}, v={:?}", name, self, v),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InstanceRef(Rc<RefCell<Instance>>);

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
    pub fn new(name: Option<Rc<String>>,
               fun_def: Rc<FunctionDefinition>,
               env: EnvironmentRef) -> ClosureRef {
        let closure = Closure::new(name, fun_def, env);

        ClosureRef(Rc::new(closure))
    }

    fn new_from_closure(closure: Closure) -> ClosureRef {
        ClosureRef(Rc::new(closure))
    }

    pub fn name(&self) -> Option<&str> {
        self.0.name()
    }

    pub fn arity(&self) -> usize {
        self.0.arity()
    }

    pub fn parameters(&self) -> &[Parameter] {
        self.0.parameters()
    }

    pub fn function_definition(&self) -> &FunctionDefinition {
        &self.0.fun_def
    }

    pub fn env(&self) -> &EnvironmentRef {
        &self.0.env
    }

    pub fn bind(&self, this_value: Value) -> ClosureRef {
        let raw_closure = self.0.bind(this_value);

        ClosureRef::new_from_closure(raw_closure)
    }
}

impl PartialEq for ClosureRef {
    fn eq(&self, other: &ClosureRef) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, PartialEq)]
pub struct Closure {
    name: Option<Rc<String>>,
    fun_def: Rc<FunctionDefinition>,
    env: EnvironmentRef,
}

impl Closure {
    pub fn new(name: Option<Rc<String>>,
               fun_def: Rc<FunctionDefinition>,
               env: EnvironmentRef) -> Closure {
        Closure {
            name,
            fun_def,
            env,
        }
    }

    pub fn name(&self) -> Option<&str> {
        self.name.as_ref().map(|string| string.as_str())
    }

    pub fn arity(&self) -> usize {
        self.fun_def.parameters.len()
    }

    pub fn parameters(&self) -> &[Parameter] {
        &self.fun_def.parameters
    }

    pub fn bind(&self, this_value: Value) -> Closure {
        // Create a new environment.
        let mut new_env = EnvironmentRef::new_with_parent(self.env.clone());
        let this_slot_index = new_env.next_slot_index();
        new_env.define_at("this", this_slot_index, this_value);

        Closure::new(self.name.as_ref().map(Rc::clone),
                     Rc::clone(&self.fun_def),
                     new_env)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // This is like inspect and tries to output the code you would need to
        // write to get the value.  This is used in the REPL.
        match self {
            // TODO: This can be more efficient.
            ArrayVal(vec) => write!(f, "[{}]",
                                    vec.borrow().iter().map(|v| v.to_string())
                                       .collect::<Vec<String>>()
                                       .join(", ")),
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

                match closure.name() {
                    Some(name) => write!(f, "fun {}({}) {{...}}", name, param_names),
                    None => write!(f, "fun({}) {{...}}", param_names),
                }
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
            ArrayVal(v) => write!(f, "ArrayVal({:?})", v),
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
        write!(f, "Closure({:?}, {:?}, Rc(...))", self.name, self.fun_def)
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

// A single line in a backtrace.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BacktraceItem {
    pub function_name: String,
    pub source_loc: SourceLoc,
}

impl BacktraceItem {
    pub fn new(function_name: String, source_loc: SourceLoc) -> BacktraceItem {
        BacktraceItem {
            function_name,
            source_loc,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Backtrace(Vec<BacktraceItem>);

impl Backtrace {
    pub fn new(backtrace_items: Vec<BacktraceItem>) -> Backtrace {
        Backtrace(backtrace_items)
    }

    pub fn iter(&self) -> Iter<BacktraceItem> {
        self.0.iter()
    }
}

// Box values to reduce size in memory.  Since it's an error, speed isn't as
// important as optimizing for the success case.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RuntimeError {
    pub source_loc: SourceLoc,
    pub message: Box<String>,
    pub backtrace: Box<Backtrace>,
}

impl RuntimeError {
    pub fn new(source_loc: SourceLoc,
               message: &str,
               backtrace: Backtrace) -> RuntimeError {
        RuntimeError {
            source_loc,
            message: Box::new(message.to_string()),
            backtrace: Box::new(backtrace),
        }
    }

    pub fn from_parse_error(source_loc: SourceLoc,
                            message: &str) -> RuntimeError {
        let items = vec![BacktraceItem::new("(parser)".to_string(), SourceLoc::default())];
        let backtrace = Backtrace::new(items);
        RuntimeError {
            source_loc,
            message: Box::new(format!("parse error: {}", message)),
            backtrace: Box::new(backtrace),
        }
    }

    pub fn from_native_error(err: NativeRuntimeError, backtrace: Backtrace) -> RuntimeError {
        RuntimeError::new(err.source_loc, &err.message, backtrace)
    }

}

impl From<ParseError> for RuntimeError {
    fn from(err: ParseError) -> RuntimeError {
        RuntimeError::from_parse_error(err.source_loc(), &err.message())
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

pub struct NativeRuntimeError {
    pub source_loc: SourceLoc,
    pub message: String,
}

impl NativeRuntimeError {
    pub fn new(source_loc: SourceLoc,
               message: &str) -> NativeRuntimeError {
        NativeRuntimeError {
            source_loc,
            message: message.to_string(),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NativeFunctionId {
    Clock,
    ArrayCreate,
    ArrayLength,
    ArrayPop,
    ArrayPush,
}

pub fn all_native_ids() -> Vec<NativeFunctionId> {
    vec![NativeFunctionId::Clock,
         NativeFunctionId::ArrayCreate,
         NativeFunctionId::ArrayLength,
         NativeFunctionId::ArrayPop,
         NativeFunctionId::ArrayPush,
    ]
}

impl fmt::Display for NativeFunctionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::NativeFunctionId::*;
        match self {
            Clock => write!(f, "clock"),
            ArrayCreate => write!(f, "array_create"),
            ArrayLength => write!(f, "array_length"),
            ArrayPop => write!(f, "array_pop"),
            ArrayPush => write!(f, "array_push"),
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
