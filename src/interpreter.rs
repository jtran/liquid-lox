use std::cell::RefCell;
use std::mem;
use std::ops::Deref;
use std::rc::Rc;
use std::time::SystemTime;

use crate::ast::*;
use crate::environment::*;
use crate::field_table::*;
use crate::memory_recycler::*;
use crate::source_loc::*;
use crate::value::*;

// Set this to true to run collection every time we allocate.
const DEBUG_MEMORY_RECYCLING: bool = false;

// This number is arbitrary and the optimal value really depends on the program
// being run.
const MIN_COLLECTION_SIZE: usize = 1024;
// This must always be >= 1 to make sense.
const HEAP_GROW_FACTOR: usize = 2;

// This is currently used only for backtrace info when there's a runtime error.
#[derive(Clone, Debug, PartialEq)]
pub struct CallFrame {
    closure_ref: ClosureRef,
    source_loc: SourceLoc,
}

pub struct Interpreter {
    env_mgr: EnvironmentManager,
    env: EnvironmentRef,
    frames: Vec<CallFrame>,
    // Environments that are reachable from the caller's program and could be
    // used, so they should not be freed for memory recycling.
    reachable_envs: Vec<EnvironmentRef>,
    recycler: Box<dyn MemoryRecycler>,
    next_collection: usize,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut env_mgr = EnvironmentManager::with_capacity(64);
        // Note: this behavior must match the Resolver.
        let globals = env_mgr.new_global();
        let mut slot_index = env_mgr.next_slot_index(globals);
        for native_id in all_native_ids() {
            let next_index = slot_index.next();
            env_mgr.define_at(globals,
                              &native_id.to_string(),
                              slot_index,
                              Value::NativeFunctionVal(native_id));
            slot_index = next_index.expect("Interpreter::new(): index for SlotIndex overflowed");
        }

        let mut reachable_envs = Vec::with_capacity(64);
        reachable_envs.push(globals.clone());

        Interpreter {
            env_mgr,
            env: globals,
            frames: Vec::with_capacity(64),
            reachable_envs,
            recycler: Box::new(MarkAndSweepRecycler::with_capacity(64)),
            next_collection: MIN_COLLECTION_SIZE,
       }
    }

    // The public interface to execute an entire program.
    pub fn interpret(&mut self, code: &ResolvedCode) -> Result<Value, RuntimeError> {
        let mut value = Value::NilVal;
        for stmt in code.statements.iter() {
            value = self.execute(stmt)?;
        }

        Ok(value)
    }

    // The public interface to execute a single statement.
    fn execute(&mut self, statement: &Stmt) -> Result<Value, RuntimeError> {
        Ok(self.exec(statement)?)
    }

    // Internal interface to execute a single statement.  This allows a more
    // diverse error type for control flow that's hidden from the public
    // interface.
    fn exec(&mut self, statement: &Stmt) -> Result<Value, ExecutionInterrupt> {
        match statement {
            Stmt::Block(statements) => {
                // Create a new environment.
                let new_env = self.new_env_from_current();

                self.exec_in_env(statements.as_slice(), new_env)
            }
            Stmt::Break(loc) => Err(ExecutionInterrupt::Break(*loc)),
            Stmt::Class(class_def) => {
                let class_slot_index = self.env_mgr.next_slot_index(self.env);
                self.env_mgr.define_at(self.env, &class_def.name, class_slot_index, Value::NilVal);

                // Superclass.
                let env_before_super = self.env.clone();
                let superclass = match &class_def.superclass {
                    None => None,
                    Some(superclass_expr) => {
                        let superclass_val = self.evaluate(superclass_expr)?;

                        match superclass_val {
                            Value::ClassVal(ref class_ref) => {
                                // Define "super".
                                let new_env = self.new_env_from_current();
                                let slot_index = self.env_mgr.next_slot_index(new_env);
                                self.env_mgr.define_at(new_env, "super", slot_index, superclass_val.clone());
                                self.reachable_envs.push(new_env);
                                self.env = new_env;

                                Some(class_ref.clone())
                            }
                            // TODO: Store and use the superclass source location.
                            _ => return Err(ExecutionInterrupt::Error(RuntimeError::new(class_def.source_loc,
                                            "Superclass must be a class.",
                                            self.backtrace()))),
                        }
                    }
                };

                let mut methods = FieldTable::new();
                let mut class_methods = FieldTable::new();
                for method in class_def.methods.iter() {
                    let closure = ClosureRef::new(Some(Rc::new(method.name.clone())), Rc::new(method.fun_def.clone()), self.env.clone());
                    let container = match method.fun_def.fun_type {
                        FunctionType::ClassMethod => &mut class_methods,
                        FunctionType::Method
                        | FunctionType::Initializer => &mut methods,
                        FunctionType::PlainFunction => panic!("interpreter::exec(): Tried to interpret method with type: {:?}", method.fun_def.fun_type),
                    };
                    container.set(&method.name, Value::ClosureVal(closure));
                }

                if superclass.is_some() {
                    self.env = env_before_super;
                    self.reachable_envs.pop();
                }

                let metaclass = ClassRef::new(&format!("{} metaclass", class_def.name),
                                              superclass,
                                              class_methods,
                                              FieldTable::new());

                let class_ref = ClassRef::new(&class_def.name,
                                              Some(metaclass),
                                              FieldTable::new(),
                                              methods);
                let var_loc = VarLoc::from(class_slot_index);
                self.env_mgr.assign_at(self.env, &class_def.name, var_loc, Value::ClassVal(class_ref))
                    .map(|_| Value::NilVal )
                    .map_err(|_| ExecutionInterrupt::Error(RuntimeError::new(class_def.source_loc,
                                    &format!("Undefined variable for class; this is probably an interpreter bug: {}", class_def.name),
                                    self.backtrace())))
            }
            Stmt::Continue(loc) => Err(ExecutionInterrupt::Continue(*loc)),
            Stmt::Expression(expr) => self.evaluate(expr).map_err(|err| err.into()),
            Stmt::Fun(fun_decl) => {
                let closure = ClosureRef::new(Some(Rc::new(fun_decl.name.clone())), Rc::new(fun_decl.fun_def.clone()), self.env.clone());
                let slot_index = self.env_mgr.next_slot_index(self.env);
                self.env_mgr.define_at(self.env, &fun_decl.name, slot_index, Value::ClosureVal(closure));

                Ok(Value::NilVal)
            }
            Stmt::If(condition, then_stmt, else_stmt) => {
                let cond_value = self.evaluate(condition)?;

                if cond_value.is_truthy() {
                    self.exec(then_stmt)
                }
                else {
                    match else_stmt {
                        Some(stmt) => self.exec(stmt),
                        None => Ok(Value::NilVal),
                    }
                }
            },
            Stmt::Print(expr) => {
                let value = self.evaluate(expr)?;
                println!("{}", value.to_runtime_string());

                Ok(Value::NilVal)
            }
            Stmt::Return(expr, _) => {
                let value = self.evaluate(expr)?;

                Err(ExecutionInterrupt::Return(value))
            }
            Stmt::Var(identifier, slot_index_cell, expr, _) => {
                let value = self.evaluate(expr)?;
                self.env_mgr.define_at(self.env, identifier, slot_index_cell.get(), value);

                Ok(Value::NilVal)
            }
            Stmt::While(condition, body) => {
                while self.evaluate(condition)?.is_truthy() {
                    let result = self.exec(body);
                    match result {
                        Ok(_) => (),
                        Err(ExecutionInterrupt::Break(_)) => break,
                        Err(ExecutionInterrupt::Continue(_)) => (),
                        // Propagate any other kind of interrupt.
                        Err(ExecutionInterrupt::Error(_)) |
                        Err(ExecutionInterrupt::Return(_)) => return result,
                    };
                }

                Ok(Value::NilVal)
            }
            Stmt::WhileIncrement(condition, body, increment) => {
                while self.evaluate(condition)?.is_truthy() {
                    let result = self.exec(body);
                    match result {
                        Ok(_) => (),
                        Err(ExecutionInterrupt::Break(_)) => break,
                        Err(ExecutionInterrupt::Continue(_)) => (),
                        // Propagate any other kind of interrupt.
                        Err(ExecutionInterrupt::Error(_)) |
                        Err(ExecutionInterrupt::Return(_)) => return result,
                    };
                    self.evaluate(increment)?;
                }

                Ok(Value::NilVal)
            }
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        use crate::value::Value::*;
        match expr {
            Expr::Assign(id, dist_cell, expr, loc) => {
                let value = self.evaluate(expr)?;
                self.env_mgr.assign_at(self.env, &id, dist_cell.get(), value.clone())
                    .map_err(|_| {
                        RuntimeError::new(*loc, &format!("Undefined variable '{}'.", &id), self.backtrace())
                    })?;

                Ok(value)
            }
            Expr::Binary(left, op, right, loc) => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;

                match op {
                    // Math operators.
                    BinaryOperator::Plus => {
                        match (&left_val, &right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 + x2)),
                            (StringVal(s1), StringVal(s2)) => {
                                Ok(StringVal(Rc::new(format!("{}{}", s1.deref(), s2.deref()))))
                            }
                            (StringVal(s1), _) => {
                                Ok(StringVal(Rc::new(format!("{}{}", s1.deref(), right_val.to_runtime_string()))))
                            }
                            (_, StringVal(s2)) => {
                                Ok(StringVal(Rc::new(format!("{}{}", left_val.to_runtime_string(), s2.deref()))))
                            }
                            (NumberVal(_), _) => Err(RuntimeError::new(*loc, "Operands must be two numbers or two strings.", self.backtrace())),
                            _ => Err(RuntimeError::new(*loc, "Operands must be two numbers or two strings.", self.backtrace())),
                        }
                    },
                    BinaryOperator::Minus => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 - x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.", self.backtrace())),
                        }
                    },
                    BinaryOperator::Multiply => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 * x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.", self.backtrace())),
                        }
                    },
                    BinaryOperator::Divide => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => {
                                if x2 == 0.0 {
                                    Err(RuntimeError::new(*loc, "attempted to divide by zero", self.backtrace()))
                                }
                                else {
                                    Ok(NumberVal(x1 / x2))
                                }
                            }
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.", self.backtrace())),
                        }
                    },
                    // Comparison operators.
                    BinaryOperator::Equal => Ok(BoolVal(left_val.is_equal(&right_val))),
                    BinaryOperator::NotEqual => Ok(BoolVal(!left_val.is_equal(&right_val))),
                    BinaryOperator::Less => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 < x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.", self.backtrace())),
                        }
                    },
                    BinaryOperator::LessEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 <= x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.", self.backtrace())),
                        }
                    },
                    BinaryOperator::Greater => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 > x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.", self.backtrace())),
                        }
                    },
                    BinaryOperator::GreaterEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 >= x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.", self.backtrace())),
                        }
                    },
                }
            }
            Expr::Call(callee, arguments, loc) => {
                let callee_val = self.evaluate(callee)?;
                let mut arg_vals: Vec<Value> = Vec::with_capacity(arguments.len());
                for arg_expr in arguments.iter() {
                    arg_vals.push(self.evaluate(arg_expr)?);
                }

                self.eval_call(callee_val, arg_vals, *loc)
            }
            Expr::Function(fun_def) => {
                let closure = ClosureRef::new(None, Rc::new(fun_def.deref().clone()), self.env.clone());

                Ok(Value::ClosureVal(closure))
            }
            Expr::Get(e, property_name, loc) => {
                let left_val = self.evaluate(e)?;

                match left_val {
                    Value::ClassVal(class_ref) => {
                        match self.class_get(&class_ref, property_name) {
                            Some(v) => Ok(v),
                            None => Err(RuntimeError::new(*loc, &format!("Undefined property '{}'.", property_name), self.backtrace())),
                        }
                    }
                    Value::InstanceVal(instance_ref) => {
                        match self.instance_get(&instance_ref, property_name) {
                            Some(v) => Ok(v),
                            None => Err(RuntimeError::new(*loc, &format!("Undefined property '{}'.", property_name), self.backtrace())),
                        }
                    }
                    _ => Err(RuntimeError::new(*loc, "Only instances have properties.", self.backtrace())),
                }
            }
            Expr::GetIndex(e, index_expr, loc) => {
                let left_val = self.evaluate(e)?;

                match left_val {
                    Value::ArrayVal(vec) => {
                        let index_val = self.evaluate(index_expr)?;

                        match index_val {
                            Value::NumberVal(x) => {
                                let truncated_x = x.trunc();
                                if x == truncated_x && x >= 0.0 {
                                    let index = truncated_x as usize;

                                    match vec.borrow().get(index) {
                                        Some(val) => Ok(val.clone()),
                                        None => Err(RuntimeError::new(*loc, "Array index out of bounds.", self.backtrace())),
                                    }
                                } else {
                                    Err(RuntimeError::new(*loc, "Array index out of bounds.", self.backtrace()))
                                }
                            }
                            _ => Err(RuntimeError::new(*loc, "Array index must be a number.", self.backtrace()))
                        }
                    }
                    _ => Err(RuntimeError::new(*loc, "Only arrays can be indexed.", self.backtrace()))
                }
            }
            Expr::Grouping(e) => self.evaluate(e),
            Expr::LiteralArray(elements) => {
                let mut vals = Vec::with_capacity(elements.len());
                for element in elements.iter() {
                    vals.push(self.evaluate(element)?);
                }

                Ok(ArrayVal(Rc::new(RefCell::new(vals))))
            }
            Expr::LiteralBool(b) => Ok(BoolVal(*b)),
            Expr::LiteralNil => Ok(NilVal),
            Expr::LiteralNumber(x) => Ok(NumberVal(*x)),
            // TODO: Should we clone every time we evaluate a literal string?
            Expr::LiteralString(s) => Ok(StringVal(Rc::new(s.clone()))),
            Expr::Logical(left, LogicalOperator::And, right) => {
                let left_val = self.evaluate(left)?;
                if left_val.is_truthy() {
                    self.evaluate(right)
                }
                else {
                    Ok(left_val)
                }
            }
            Expr::Logical(left, LogicalOperator::Or, right) => {
                let left_val = self.evaluate(left)?;
                if left_val.is_truthy() {
                    Ok(left_val)
                }
                else {
                    self.evaluate(right)
                }
            }
            Expr::Set(object_expr, property_name, e, loc) => {
                let left_val = self.evaluate(object_expr)?;

                match left_val {
                    Value::ClassVal(mut class_ref) => {
                        let value = self.evaluate(e)?;
                        class_ref.set(property_name, value.clone());

                        Ok(value)
                    }
                    Value::InstanceVal(mut instance_ref) => {
                        let value = self.evaluate(e)?;
                        instance_ref.set(property_name, value.clone());

                        Ok(value)
                    }
                    _ => Err(RuntimeError::new(*loc, "Only instances have fields.", self.backtrace())),
                }
            }
            Expr::SetIndex(array_expr, index, rhs, loc) => {
                let array_val = self.evaluate(array_expr)?;

                match array_val {
                    Value::ArrayVal(vec) => {
                        let index_val = self.evaluate(index)?;
                        match index_val {
                            Value::NumberVal(x) => {
                                let truncated_x = x.trunc();
                                if x == truncated_x && x >= 0.0 {
                                    let index = truncated_x as usize;
                                    let value = self.evaluate(rhs)?;

                                    if index < vec.borrow().len() {
                                        vec.borrow_mut()[index] = value.clone();

                                        Ok(value)
                                    } else {
                                        Err(RuntimeError::new(*loc, "Array index out of bounds.", self.backtrace()))
                                    }
                                } else {
                                    Err(RuntimeError::new(*loc, "Array index out of bounds.", self.backtrace()))
                                }
                            }
                            _ => Err(RuntimeError::new(*loc, "Array index must be a number.", self.backtrace())),
                        }
                    }
                    _ => Err(RuntimeError::new(*loc, "Only arrays can be indexed.", self.backtrace())),
                }
            }
            Expr::Super(super_dist_cell, id, loc) => {
                let super_var_loc = super_dist_cell.get();
                let superclass = match self.env_mgr.get_at(self.env, "super", super_var_loc) {
                    // Interpreter bug?
                    None => return Err(RuntimeError::new(*loc, "Undefined variable: super", self.backtrace())),
                    Some(Value::ClassVal(class)) => class,
                    Some(_) => return Err(RuntimeError::new(*loc, "super didn't evaluate to a class", self.backtrace())),
                };

                // Instance is always defined one scope before super.
                let this_dist = match super_var_loc.distance().checked_sub(1) {
                    Some(x) => x,
                    None => panic!("location of \"this\" for super expression cannot be computed; probably an interpreter bug; super_var_loc={:?}", super_var_loc),
                };
                let this_var_loc = VarLoc::new(this_dist,
                                               super_var_loc.index());

                match self.env_mgr.get_at(self.env, id, this_var_loc) {
                    // Interpreter bug?
                    None => Err(RuntimeError::new(*loc, "Undefined variable \"this\" when evaluating super expression", self.backtrace())),
                    Some(Value::ClassVal(_)) => {
                        self.bound_class_method(&superclass, id)
                            .ok_or_else(|| RuntimeError::new(*loc, &format!("Undefined property '{}'.", id), self.backtrace()))
                    }
                    Some(Value::InstanceVal(instance_ref)) => {
                        self.bound_method(&superclass, id, instance_ref)
                            .ok_or_else(|| RuntimeError::new(*loc, &format!("Undefined property '{}'.", id), self.backtrace()))
                    }
                    // Interpreter bug?
                    Some(_) => Err(RuntimeError::new(*loc, "\"this\" in super expression didn't evaluate to an instance or class", self.backtrace())),
                }
            }
            Expr::Variable(id, dist_cell, loc) => {
                self.env_mgr.get_at(self.env, id, dist_cell.get())
                    // In the case that the variable is not in the environment,
                    // generate a runtime error.
                    .ok_or_else(|| RuntimeError::new(*loc, &format!("Undefined variable '{}'.", id), self.backtrace()))
            }
            Expr::Unary(op, e, loc) => {
                let v = self.evaluate(e)?;

                match op {
                    UnaryOperator::Minus => {
                        match v {
                            NumberVal(x) => Ok(NumberVal(-x)),
                            _ => Err(RuntimeError::new(*loc, "Operand must be a number.", self.backtrace())),
                        }
                    },
                    UnaryOperator::Not => Ok(BoolVal(!v.is_truthy())),
                }
            }
        }
    }

    // Executes statements in the given environment.
    fn exec_in_env<S>(&mut self, statements: &[S], env: EnvironmentRef)
        -> Result<Value, ExecutionInterrupt>
        where S: AsRef<Stmt>
    {
        // Add the environment as a root for memory recycling.
        self.reachable_envs.push(env);

        // Do a memory collection if needed.
        self.maybe_collect_memory();

        // Use the new environment.
        let enclosing = mem::replace(&mut self.env, env);

        // Execute statements.
        let mut result = Ok(Value::NilVal);
        for statement in statements {
            result = self.exec(statement.as_ref());
            if result.is_err() {
                break;
            }
        }

        // Restore previous environment.
        self.env = enclosing;

        // Environment is no longer a root for memory recycling.
        self.reachable_envs.pop();

        result
    }

    // Bind a closure to a value of `this` so that you can call a bound method.
    fn bind_closure(&mut self, closure: &ClosureRef, this_value: Value) -> ClosureRef {
        // Create a new environment.
        let new_env = self.new_env_with_parent(*closure.env());
        let this_slot_index = self.env_mgr.next_slot_index(new_env);
        self.env_mgr.define_at(new_env, "this", this_slot_index, this_value);

        closure.with_env(new_env)
    }

    // Get member from an instance.
    fn instance_get(&mut self, instance: &InstanceRef, name: &str) -> Option<Value> {
        let v = instance.field_value(name);
        if v.is_some() {
            return v;
        }

        self.bound_method(&instance.class(), name, instance.clone())
    }

    // Get member from a class.
    fn class_get(&mut self, class_ref: &ClassRef, name: &str) -> Option<Value> {
        let v = class_ref.field_value(name);
        if v.is_some() {
            return v;
        }

        self.bound_class_method(&class_ref, name)
    }

    fn bound_method(&mut self,
                    cls: &ClassRef,
                    name: &str,
                    this_instance_ref: InstanceRef) -> Option<Value> {
        match cls.find_method(name) {
            None => None,
            Some(Value::ClosureVal(closure)) => {
                let bound_meth = self.bind_closure(&closure, Value::InstanceVal(this_instance_ref));

                Some(Value::ClosureVal(bound_meth))
            }
            Some(v) => panic!("Accessing a property and looking up a method resulted in a non-closure value: name={}, class={:?}, v={:?}", name, cls, v),
        }
    }

    fn bound_class_method(&mut self,
                          class_ref: &ClassRef,
                          name: &str) -> Option<Value> {
        match class_ref.find_class_method(name) {
            None => None,
            Some(Value::ClosureVal(closure)) => {
                let bound_meth = self.bind_closure(&closure, Value::ClassVal(class_ref.clone()));

                Some(Value::ClosureVal(bound_meth))
            }
            Some(v) => panic!("Accessing a property and looking up a method resulted in a non-closure value: name={}, class={:?}, v={:?}", name, class_ref, v),
        }
    }

    fn eval_call(&mut self, callee: Value, args: Vec<Value>, loc: SourceLoc)
        -> Result<Value, RuntimeError>
    {
        match callee {
            Value::NativeFunctionVal(id) => {
                let native_function = NativeFunction::new(id);
                self.check_call_arity(native_function.arity(), args.len(), &loc)?;

                native_function.call(args, loc)
                    .map_err(|e| RuntimeError::from_native_error(e, self.backtrace()))
            }
            Value::ClassVal(class_ref) => {
                let instance_val = Value::InstanceVal(InstanceRef::new(class_ref.clone()));

                // Call the initializer.
                match class_ref.find_method("init") {
                    None => {
                        // When no initializer is defined, instantiating should
                        // take zero arguments.
                        self.check_call_arity(0, args.len(), &loc)?;
                        return Ok(instance_val);
                    }
                    Some(Value::ClosureVal(closure)) => {
                        self.check_call_arity(closure.arity(), args.len(), &loc)?;

                        let bound_method = self.bind_closure(&closure, instance_val);
                        return self.eval_call(Value::ClosureVal(bound_method), args, loc);
                    }
                    Some(v) => return Err(RuntimeError::new(loc, &format!("The \"init\" property of a class should be a function, but it isn't: {}", v), self.backtrace())),
                };
            }
            Value::ClosureVal(closure_ref) => {
                let fun_def = closure_ref.function_definition();
                self.check_call_arity(fun_def.parameters.len(), args.len(), &loc)?;
                // Create call frame.
                let frame = CallFrame {
                    closure_ref: closure_ref.clone(),
                    source_loc: loc,
                };
                self.frames.push(frame);
                // Create a new environment, enclosed by closure's environment.
                let new_env = self.new_env_for_call(closure_ref.env().clone());
                // Bind parameters to argument values.
                let mut slot_index = self.env_mgr.next_slot_index(new_env);
                for (parameter, arg) in fun_def.parameters.iter().zip(args) {
                    let next_index = slot_index.next();
                    self.env_mgr.define_at(new_env, &parameter.name, slot_index, arg);
                    slot_index = next_index.expect("eval_call: index for SlotIndex overflowed");
                }
                // Execute function body in new environment.
                let return_value_result = self.exec_in_env(fun_def.body.as_slice(), new_env);

                // Pop the call frame.
                self.frames.pop();

                match fun_def.fun_type {
                    // For initializers, always return the instance.
                    FunctionType::Initializer => {
                        // TODO: There must be a better way to find the
                        // instance.
                        let this_var_loc = VarLoc::new(0, 0);

                        match self.env_mgr.get_at(*closure_ref.env(), "this", this_var_loc) {
                            Some(value) => Ok(value),
                            None => panic!("Couldn't find 'this' in initializer environment at index 0: {:?}", fun_def),
                        }
                    }
                    // For all other function types, use the return value.
                    FunctionType::PlainFunction
                    | FunctionType::Method
                    | FunctionType::ClassMethod =>
                        self.unwrap_return_value(return_value_result),
                }
            }
            _ => Err(RuntimeError::new(loc, "Can only call functions and classes.", self.backtrace())),
        }
    }

    fn check_call_arity(&self, arity: usize, num_args: usize, loc: &SourceLoc)
        -> Result<(), RuntimeError>
    {
        if num_args != arity {
            return Err(RuntimeError::new(*loc, &format!("Expected {} arguments but got {}.", arity, num_args), self.backtrace()));
        }

        Ok(())
    }

    // Given an execution interrupt, unwrap any value from a return statement.
    fn unwrap_return_value(&self, result: Result<Value, ExecutionInterrupt>)
        -> Result<Value, RuntimeError>
    {
        match result {
            Err(ExecutionInterrupt::Return(value)) => Ok(value),
            // In the case that there was no return, discard the result and
            // return nil.  This disables implicit return values.
            _ => result.and(Ok(Value::NilVal)).map_err(|e| e.into()),
        }
    }

    fn new_env_from_current(&mut self) -> EnvironmentRef {
        // TODO: We should be able to optimize away creating an entire
        // environment since this isn't for a function call.
        self.new_env_with_parent(self.env.clone())
    }

    fn new_env_with_parent(&mut self, enclosing: EnvironmentRef) -> EnvironmentRef {
        self.env_mgr.new_with_parent(enclosing)
    }

    fn new_env_for_call(&mut self, enclosing: EnvironmentRef) -> EnvironmentRef {
        self.new_env_with_parent(enclosing)
    }

    fn maybe_collect_memory(&mut self) {
        // Check if we need to collect unreachable memory.
        let num_layers = self.env_mgr.num_layers();
        if DEBUG_MEMORY_RECYCLING || num_layers >= self.next_collection {
            // Run a collection.
            if DEBUG_MEMORY_RECYCLING {
                eprintln!("Running collection; num_layers={:?}", num_layers);
            }
            self.recycler.collect(&mut self.env_mgr, &self.reachable_envs);

            self.next_collection = num_layers.saturating_mul(HEAP_GROW_FACTOR)
                                             .max(MIN_COLLECTION_SIZE);
            if DEBUG_MEMORY_RECYCLING {
                eprintln!("  Setting next collection to {}", self.next_collection);
            }
        }
    }

    fn backtrace(&self) -> Backtrace {
        let mut items = Vec::with_capacity(self.frames.len());
        for frame in self.frames.iter().rev() {
            let fun_name = frame.closure_ref.name().map(str::to_string)
                                .unwrap_or_else(|| "(anonymous)".to_string());
            items.push(BacktraceItem::new(fun_name,
                                          frame.source_loc))
        }

        Backtrace::new(items)
    }
}

struct NativeFunction {
    id: NativeFunctionId
}

impl NativeFunction {
    pub fn new(id: NativeFunctionId) -> NativeFunction {
        NativeFunction { id }
    }

    pub fn arity(&self) -> usize {
        match self.id {
            NativeFunctionId::Clock => 0,
            NativeFunctionId::ArrayCreate => 2,
            NativeFunctionId::ArrayLength => 1,
            NativeFunctionId::ArrayPop => 1,
            NativeFunctionId::ArrayPush => 2,
        }
    }

    pub fn call(&self, args: Vec<Value>, loc: SourceLoc) -> Result<Value, NativeRuntimeError> {
        match self.id {
            NativeFunctionId::Clock => {
                const MILLIS_PER_SEC: u64 = 1000;
                let sys_time = SystemTime::now();

                match sys_time.duration_since(SystemTime::UNIX_EPOCH) {
                    Ok(duration) => {
                        let secs = u128::from(duration.as_secs());
                        let millis = u128::from(duration.subsec_millis());
                        let combined_millis = secs * u128::from(MILLIS_PER_SEC) + millis;

                        Ok(Value::NumberVal(combined_millis as f64 / 1000.0))
                    }
                    Err(_) => Err(NativeRuntimeError::new(loc, "Unable to get system time since now is before the epoch")),
                }
            }
            NativeFunctionId::ArrayCreate => {
                match &args[0] {
                    Value::NumberVal(x) => {
                        let truncated_x = x.trunc();
                        if *x == truncated_x && *x >= 0.0 {
                            let length = truncated_x as usize;
                            let mut vec = Vec::with_capacity(length);
                            vec.resize(length, args[1].clone());

                            Ok(Value::ArrayVal(Rc::new(RefCell::new(vec))))
                        } else {
                            Err(NativeRuntimeError::new(loc, "Array length must be a non-negative integer."))
                        }
                    }
                    _ => Err(NativeRuntimeError::new(loc, "Array create expects number and value.")),
                }
            }
            NativeFunctionId::ArrayLength => {
                match &args[0] {
                    Value::ArrayVal(vec) => Ok(Value::NumberVal(vec.borrow().len() as f64)),
                    _ => Err(NativeRuntimeError::new(loc, "Can only get length of an array.")),
                }
            }
            NativeFunctionId::ArrayPop => {
                match &args[0] {
                    Value::ArrayVal(vec) => {
                        match vec.borrow_mut().pop() {
                            Some(v) => Ok(v),
                            None => Err(NativeRuntimeError::new(loc, "Cannot pop on an empty array.")),
                        }
                    }
                    _ => Err(NativeRuntimeError::new(loc, "Can only pop on an array.")),
                }
            }
            NativeFunctionId::ArrayPush => {
                match &args[0] {
                    Value::ArrayVal(vec) => {
                        vec.borrow_mut().push(args[1].clone());

                        Ok(Value::NilVal)
                    }
                    _ => Err(NativeRuntimeError::new(loc, "Can only push on an array.")),
                }
            }
        }
    }
}
