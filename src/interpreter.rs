use std::cell::RefCell;
use std::mem;
use std::ops::Deref;
use std::rc::Rc;
use std::time::SystemTime;

use crate::ast::*;
use crate::environment::*;
use crate::field_table::*;
use crate::source_loc::*;
use crate::value::*;

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        // Note: this behavior must match the Resolver.
        let mut globals = Environment::new_global();
        let mut frame_index = globals.next_frame_index();
        for native_id in all_native_ids() {
            let next_index = frame_index.next();
            globals.define_at(&native_id.to_string(),
                              frame_index,
                              Value::NativeFunctionVal(native_id));
            frame_index = next_index.expect("Interpreter::new(): index for FrameIndex overflowed");
        }

        Interpreter {
            env: Rc::new(RefCell::new(globals)),
        }
    }

    // The public interface to execute an entire program.
    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<Value, RuntimeError> {
        let mut value = Value::NilVal;
        for stmt in statements.iter() {
            value = self.execute(stmt)?;
        }

        Ok(value)
    }

    // The public interface to execute a single statement.
    pub fn execute(&mut self, statement: &Stmt) -> Result<Value, RuntimeError> {
        Ok(self.exec(statement)?)
    }

    // Internal interface to execute a single statement.  This allows a more
    // diverse error type for control flow that's hidden from the public
    // interface.
    fn exec(&mut self, statement: &Stmt) -> Result<Value, ExecutionInterrupt> {
        match statement {
            Stmt::Block(statements) => {
                // Create a new environment.
                let new_env = Rc::new(RefCell::new(self.new_env_from_current()));

                self.exec_in_env(statements.as_slice(), new_env)
            }
            Stmt::Break(loc) => Err(ExecutionInterrupt::Break(*loc)),
            Stmt::Class(class_def) => {
                let class_frame_index = {
                    // Scope is to end mutable borrow of self.
                    let mut env = self.env.deref().borrow_mut();
                    let frame_index = env.next_frame_index();

                    env.define_at(&class_def.name, frame_index, Value::NilVal);

                    frame_index
                };

                // Superclass.
                let env_before_super = Rc::clone(&self.env);
                let superclass = match &class_def.superclass {
                    None => None,
                    Some(superclass_expr) => {
                        let superclass_val = self.evaluate(superclass_expr)?;

                        match superclass_val {
                            Value::ClassVal(ref class_ref) => {
                                // Define "super".
                                let mut new_env = self.new_env_from_current();
                                let frame_index = new_env.next_frame_index();
                                new_env.define_at("super", frame_index, superclass_val.clone());
                                self.env = Rc::new(RefCell::new(new_env));

                                Some(class_ref.clone())
                            }
                            // TODO: Store and use the superclass source location.
                            _ => return Err(ExecutionInterrupt::Error(RuntimeError::new(class_def.source_loc,
                                            "Superclass must be a class."))),
                        }
                    }
                };

                let mut methods = FieldTable::new();
                let mut class_methods = FieldTable::new();
                for method in class_def.methods.iter() {
                    let closure = ClosureRef::new(Rc::new(method.clone()), Rc::clone(&self.env));
                    let container = match method.fun_type {
                        FunctionType::ClassMethod => &mut class_methods,
                        FunctionType::Method
                        | FunctionType::Initializer => &mut methods,
                        FunctionType::PlainFunction => panic!("interpreter::exec(): Tried to interpret method with type: {:?}", method.fun_type),
                    };
                    container.set(&method.name, Value::ClosureVal(closure));
                }

                if superclass.is_some() {
                    self.env = env_before_super;
                }

                let class_ref = ClassRef::new(&class_def.name,
                                              superclass,
                                              class_methods,
                                              methods);
                let mut env = self.env.deref().borrow_mut();
                let var_loc = VarLoc::from(class_frame_index);
                env.assign_at(&class_def.name, var_loc, Value::ClassVal(class_ref))
                    .map(|_| Value::NilVal )
                    .map_err(|_| ExecutionInterrupt::Error(RuntimeError::new(class_def.source_loc,
                                    &format!("Undefined variable for class; this is probably an interpreter bug: {}", class_def.name))))
            }
            Stmt::Continue(loc) => Err(ExecutionInterrupt::Continue(*loc)),
            Stmt::Expression(expr) => self.evaluate(expr).map_err(|err| err.into()),
            Stmt::Fun(fun_def) => {
                let closure = ClosureRef::new(Rc::new(fun_def.clone()), Rc::clone(&self.env));
                let mut env = self.env.deref().borrow_mut();
                let frame_index = env.next_frame_index();
                env.define_at(&fun_def.name, frame_index, Value::ClosureVal(closure));

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
            Stmt::Var(identifier, frame_index_cell, expr, _) => {
                let value = self.evaluate(expr)?;
                let mut env = self.env.deref().borrow_mut();
                env.define_at(identifier, frame_index_cell.get(), value);

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

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        use crate::value::Value::*;
        match expr {
            Expr::Assign(id, dist_cell, expr, loc) => {
                let value = self.evaluate(expr)?;
                let mut env = self.env.deref().borrow_mut();
                env.assign_at(&id, dist_cell.get(), value.clone())
                    .map_err(|_| {
                        RuntimeError::new(*loc, &format!("Undefined variable '{}'.", &id))
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
                            (NumberVal(_), _) => Err(RuntimeError::new(*loc, "Operands must be two numbers or two strings.")),
                            _ => Err(RuntimeError::new(*loc, "Operands must be two numbers or two strings.")),
                        }
                    },
                    BinaryOperator::Minus => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 - x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.")),
                        }
                    },
                    BinaryOperator::Multiply => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 * x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.")),
                        }
                    },
                    BinaryOperator::Divide => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => {
                                if x2 == 0.0 {
                                    Err(RuntimeError::new(*loc, "attempted to divide by zero"))
                                }
                                else {
                                    Ok(NumberVal(x1 / x2))
                                }
                            }
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.")),
                        }
                    },
                    // Comparison operators.
                    BinaryOperator::Equal => Ok(BoolVal(left_val.is_equal(&right_val))),
                    BinaryOperator::NotEqual => Ok(BoolVal(!left_val.is_equal(&right_val))),
                    BinaryOperator::Less => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 < x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.")),
                        }
                    },
                    BinaryOperator::LessEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 <= x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.")),
                        }
                    },
                    BinaryOperator::Greater => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 > x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.")),
                        }
                    },
                    BinaryOperator::GreaterEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 >= x2)),
                            _ => Err(RuntimeError::new(*loc, "Operands must be numbers.")),
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
            Expr::Get(e, property_name, loc) => {
                let left_val = self.evaluate(e)?;

                match left_val {
                    Value::ClassVal(class_ref) => {
                        match class_ref.get(property_name) {
                            Some(v) => Ok(v),
                            None => Err(RuntimeError::new(*loc, &format!("Undefined property '{}'.", property_name))),
                        }
                    }
                    Value::InstanceVal(instance_ref) => {
                        match instance_ref.get(property_name) {
                            Some(v) => Ok(v),
                            None => Err(RuntimeError::new(*loc, &format!("Undefined property '{}'.", property_name))),
                        }
                    }
                    _ => Err(RuntimeError::new(*loc, "Only instances have properties.")),
                }
            }
            Expr::Grouping(e) => self.evaluate(e),
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
                    Value::InstanceVal(mut instance_ref) => {
                        let value = self.evaluate(e)?;
                        instance_ref.set(property_name, value.clone());

                        Ok(value)
                    }
                    _ => Err(RuntimeError::new(*loc, "Only instances have fields.")),
                }
            }
            Expr::Super(super_dist_cell, id, loc) => {
                let env = self.env.deref().borrow();

                let super_var_loc = super_dist_cell.get();
                let superclass = match env.get_at("super", super_var_loc) {
                    // Interpreter bug?
                    None => return Err(RuntimeError::new(*loc, "Undefined variable: super")),
                    Some(Value::ClassVal(class)) => class,
                    Some(_) => return Err(RuntimeError::new(*loc, "super didn't evaluate to a class")),
                };

                // Instance is always defined one frame before super.
                let this_dist = match super_var_loc.distance().checked_sub(1) {
                    Some(x) => x,
                    None => panic!("location of \"this\" for super expression cannot be computed; probably an interpreter bug; super_var_loc={:?}", super_var_loc),
                };
                let this_var_loc = VarLoc::new(this_dist,
                                               super_var_loc.index());

                match env.get_at(id, this_var_loc) {
                    // Interpreter bug?
                    None => Err(RuntimeError::new(*loc, "Undefined variable \"this\" when evaluating super expression")),
                    Some(Value::ClassVal(class_ref)) => {
                        superclass.bound_class_method(id, class_ref)
                            .ok_or_else(|| RuntimeError::new(*loc, &format!("Undefined property '{}'.", id)))
                    }
                    Some(Value::InstanceVal(instance_ref)) => {
                        superclass.bound_method(id, instance_ref)
                            .ok_or_else(|| RuntimeError::new(*loc, &format!("Undefined property '{}'.", id)))
                    }
                    // Interpreter bug?
                    Some(_) => Err(RuntimeError::new(*loc, "\"this\" in super expression didn't evaluate to an instance or class")),
                }
            }
            Expr::Variable(id, dist_cell, loc) => {
                let env = self.env.deref().borrow();

                env.get_at(id, dist_cell.get())
                    // In the case that the variable is not in the environment,
                    // generate a runtime error.
                    .ok_or_else(|| RuntimeError::new(*loc, &format!("Undefined variable '{}'.", id)))
            }
            Expr::Unary(op, e, loc) => {
                let v = self.evaluate(e)?;

                match op {
                    UnaryOperator::Minus => {
                        match v {
                            NumberVal(x) => Ok(NumberVal(-x)),
                            _ => Err(RuntimeError::new(*loc, "Operand must be a number.")),
                        }
                    },
                    UnaryOperator::Not => Ok(BoolVal(!v.is_truthy())),
                }
            }
        }
    }

    // Executes statements in the given environment.
    fn exec_in_env<S>(&mut self, statements: &[S], env: Rc<RefCell<Environment>>)
        -> Result<Value, ExecutionInterrupt>
        where S: AsRef<Stmt>
    {
        // Create a new environment.
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

        result
    }

    fn eval_call(&mut self, callee: Value, args: Vec<Value>, loc: SourceLoc)
        -> Result<Value, RuntimeError>
    {
        match callee {
            Value::NativeFunctionVal(id) => {
                let native_function = NativeFunction::new(id);
                self.check_call_arity(native_function.arity(), args.len(), &loc)?;

                native_function.call(args, loc)
            }
            Value::ClassVal(class_ref) => {
                let instance_val = Value::InstanceVal(InstanceRef::new(class_ref.clone()));

                // Call the initializer.
                match class_ref.find_method("init") {
                    None => {
                        // When no initializer is defined, instantiating should
                        // take zero arguments.
                        self.check_call_arity(0, args.len(), &loc)?;
                    }
                    Some(Value::ClosureVal(closure)) => {
                        self.check_call_arity(closure.arity(), args.len(), &loc)?;

                        let bound_method = closure.bind(instance_val.clone());
                        self.eval_call(Value::ClosureVal(bound_method), args, loc)?;
                    }
                    Some(v) => return Err(RuntimeError::new(loc, &format!("The \"init\" property of a class should be a function, but it isn't: {}", v))),
                };

                Ok(instance_val)
            }
            Value::ClosureVal(closure_ref) => {
                let fun_def = closure_ref.function_definition();
                self.check_call_arity(fun_def.parameters.len(), args.len(), &loc)?;
                // Create a new environment, enclosed by closure's environment.
                let mut new_env = Environment::new_with_parent(Rc::clone(closure_ref.env()));
                // Bind parameters to argument values.
                let mut frame_index = new_env.next_frame_index();
                for (parameter, arg) in fun_def.parameters.iter().zip(args) {
                    let next_index = frame_index.next();
                    new_env.define_at(&parameter.name, frame_index, arg);
                    frame_index = next_index.expect("eval_call: index for FrameIndex overflowed");
                }
                // Execute function body in new environment.
                let new_env_sptr = Rc::new(RefCell::new(new_env));
                let return_value_result = self.exec_in_env(fun_def.body.as_slice(), new_env_sptr);

                match fun_def.fun_type {
                    // For initializers, always return the instance.
                    FunctionType::Initializer => {
                        let env = closure_ref.env().deref().borrow();
                        // TODO: There must be a better way to find the
                        // instance.
                        let this_var_loc = VarLoc::new(0, 0);

                        match env.get_at("this", this_var_loc) {
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
            _ => Err(RuntimeError::new(loc, "Can only call functions and classes.")),
        }
    }

    fn check_call_arity(&self, arity: usize, num_args: usize, loc: &SourceLoc)
        -> Result<(), RuntimeError>
    {
        if num_args != arity {
            return Err(RuntimeError::new(*loc, &format!("Expected {} arguments but got {}.", arity, num_args)));
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

    fn new_env_from_current(&self) -> Environment {
        Environment::new_with_parent(Rc::clone(&self.env))
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
        }
    }

    pub fn call(&self, _args: Vec<Value>, loc: SourceLoc) -> Result<Value, RuntimeError> {
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
                    Err(_) => Err(RuntimeError::new(loc, "Unable to get system time since now is before the epoch")),
                }
            }
        }
    }
}
