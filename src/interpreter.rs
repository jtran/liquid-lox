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
        for native_id in all_native_ids() {
            globals.define(&native_id.to_string(),
                           Value::NativeFunctionVal(native_id));
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
                let index = {
                    // Scope is to end mutable borrow of self.
                    let mut env = self.env.deref().borrow_mut();

                    env.define(&class_def.name, Value::NilVal)
                };
                let var_loc = VarLoc::new(0, index);

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
                                new_env.define("super", superclass_val.clone());
                                self.env = Rc::new(RefCell::new(new_env));

                                Some(class_ref.clone())
                            }
                            // TODO: Store and use the superclass source location.
                            _ => return Err(ExecutionInterrupt::Error(RuntimeError::new_with_details(class_def.source_loc,
                                            "Superclass must be a class.",
                                            &format!("Superclass must be a class; instead found: {}", superclass_val)))),
                        }
                    }
                };

                let mut methods = FieldTable::new();
                let mut class_methods = FieldTable::new();
                for method in class_def.methods.iter() {
                    let closure = ClosureRef::new(Rc::new(method.clone()), Rc::clone(&self.env));
                    let container = if method.fun_type == FunctionType::ClassMethod {
                        &mut class_methods
                    }
                    else {
                        &mut methods
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
                env.assign_at(&class_def.name, var_loc, Value::ClassVal(class_ref))
                    .map(|_| Value::NilVal )
                    .map_err(|_| ExecutionInterrupt::Error(RuntimeError::new(class_def.source_loc,
                                    &format!("Undefined variable for class; this is probably an interpreter bug: {}", class_def.name))))
            }
            Stmt::Expression(expr) => self.evaluate(expr).map_err(|err| err.into()),
            Stmt::Fun(fun_def) => {
                let closure = ClosureRef::new(Rc::new(fun_def.clone()), Rc::clone(&self.env));
                let mut env = self.env.deref().borrow_mut();
                env.define(&fun_def.name, Value::ClosureVal(closure));

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
            Stmt::Var(identifier, expr) => {
                let value = self.evaluate(expr)?;
                let mut env = self.env.deref().borrow_mut();
                env.define(identifier, value);

                Ok(Value::NilVal)
            }
            Stmt::While(condition, body) => {
                while self.evaluate(condition)?.is_truthy() {
                    let result = self.exec(body);
                    match result {
                        Ok(_) => (),
                        Err(ExecutionInterrupt::Break(_)) => break,
                        // Propagate any other kind of interrupt.
                        Err(ExecutionInterrupt::Error(_)) |
                        Err(ExecutionInterrupt::Return(_)) => return result,
                    };
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
                let left_type = left_val.runtime_type();
                let right_val = self.evaluate(right)?;
                let right_type = right_val.runtime_type();

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
                            (NumberVal(_), _) => Err(RuntimeError::new_with_details(*loc, "Operands must be two numbers or two strings.", &format!("expected number but found {} evaluating plus in expression: {:?}", right_type, expr))),
                            _ => Err(RuntimeError::new_with_details(*loc, "Operands must be two numbers or two strings.", &format!("expected number or string but found {} evaluating plus in expression: {:?}", left_type, expr))),
                        }
                    },
                    BinaryOperator::Minus => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 - x2)),
                            _ => Err(RuntimeError::new_with_details(*loc, "Operands must be numbers.", &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::Multiply => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 * x2)),
                            _ => Err(RuntimeError::new_with_details(*loc, "Operands must be numbers.", &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
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
                            _ => Err(RuntimeError::new_with_details(*loc, "Operands must be numbers.", &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    // Comparison operators.
                    BinaryOperator::Equal => Ok(BoolVal(left_val.is_equal(&right_val))),
                    BinaryOperator::NotEqual => Ok(BoolVal(!left_val.is_equal(&right_val))),
                    BinaryOperator::Less => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 < x2)),
                            _ => Err(RuntimeError::new_with_details(*loc, "Operands must be numbers.", &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::LessEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 <= x2)),
                            _ => Err(RuntimeError::new_with_details(*loc, "Operands must be numbers.", &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::Greater => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 > x2)),
                            _ => Err(RuntimeError::new_with_details(*loc, "Operands must be numbers.", &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::GreaterEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 >= x2)),
                            _ => Err(RuntimeError::new_with_details(*loc, "Operands must be numbers.", &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
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
                    _ => Err(RuntimeError::new_with_details(*loc, "Only instances have properties.", &format!("Only instances and classes have properties but found {} with type {}", left_val, left_val.runtime_type()))),
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
                let value = self.evaluate(e)?;
                let left_val = self.evaluate(object_expr)?;

                match left_val {
                    Value::InstanceVal(mut instance_ref) => {
                        instance_ref.set(property_name, value.clone());

                        Ok(value)
                    }
                    _ => Err(RuntimeError::new_with_details(*loc, "Only instances have fields.", &format!("Only instances have fields to set but found {} with type {}", left_val, left_val.runtime_type()))),
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
                let right_type = v.runtime_type();

                match op {
                    UnaryOperator::Minus => {
                        match v {
                            NumberVal(x) => Ok(NumberVal(-x)),
                            _ => Err(RuntimeError::new_with_details(*loc, "Operand must be a number.", &format!("expected number but found {} evaluating expression: {:?}", right_type, expr))),
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
                self.check_call_arity(fun_def.parameters.len(), args.len(), &fun_def.source_loc)?;
                // Create a new environment, enclosed by closure's environment.
                let mut new_env = Environment::new_with_parent(Rc::clone(closure_ref.env()));
                // Bind parameters to argument values.
                for (parameter, arg) in fun_def.parameters.iter().zip(args) {
                    new_env.define(&parameter.name, arg);
                }
                // Execute function body in new environment.
                let new_env_sptr = Rc::new(RefCell::new(new_env));
                let return_value_result = self.exec_in_env(fun_def.body.as_slice(), new_env_sptr);

                self.unwrap_return_value(return_value_result)
            }
            _ => Err(RuntimeError::new_with_details(loc, "Can only call functions and classes.", &format!("You can only call functions and classes, but you tried to call: {}", callee))),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;
    use crate::resolver;
    use crate::value::Value::*;

    fn interpret(code: &str) -> Result<Value, RuntimeError> {
        let mut ast = parse(code)?;
        resolver::resolve(&mut ast).map_err(|e| ParseError::from(e))?;
        let mut interpreter = Interpreter::new();

        interpreter.interpret(ast)
    }

    fn interpret_repl_line(code: &str) -> Result<Value, RuntimeError> {
        let mut ast = parse_repl_line(code)?;
        resolver::resolve(&mut ast).map_err(|e| ParseError::from(e))?;
        let mut interpreter = Interpreter::new();

        interpreter.interpret(ast)
    }

    fn eval(code: &str) -> Result<Value, RuntimeError> {
        let mut ast = parse_expression(code)?;
        resolver::resolve_expression(&mut ast).map_err(|e| ParseError::from(e))?;
        let mut interpreter = Interpreter::new();

        interpreter.evaluate(&ast)
    }

    #[test]
    fn test_eval_literals() {
        assert_eq!(eval("42"), Ok(NumberVal(42.0)));
        assert_eq!(eval("\"hello\""), Ok(StringVal(Rc::new("hello".to_string()))));
        assert_eq!(eval("true"), Ok(BoolVal(true)));
        assert_eq!(eval("false"), Ok(BoolVal(false)));
        assert_eq!(eval("nil"), Ok(NilVal));
    }

    #[test]
    fn test_eval_binary_ops() {
        assert_eq!(eval("40 + 2"), Ok(NumberVal(42.0)));
        assert_eq!(eval("\"foo\" + \"bar\""), Ok(StringVal(Rc::new("foobar".to_string()))));
        assert_eq!(eval("40 - 10"), Ok(NumberVal(30.0)));
        assert_eq!(eval("7 * 3"), Ok(NumberVal(21.0)));
        assert_eq!(eval("10 / 2"), Ok(NumberVal(5.0)));
    }

    #[test]
    fn test_eval_divide_by_zero() {
        let loc = SourceLoc::new(1, 3);
        assert_eq!(eval("1 / 0"), Err(RuntimeError::new(loc, "attempted to divide by zero")));
    }

    #[test]
    fn test_eval_comparison() {
        assert_eq!(eval("true == true"), Ok(BoolVal(true)));
        assert_eq!(eval("true == 32"), Ok(BoolVal(false)));
        assert_eq!(eval("2 < 3"), Ok(BoolVal(true)));
        assert_eq!(eval("2 > 3"), Ok(BoolVal(false)));
    }

    #[test]
    fn test_eval_unary_ops() {
        assert_eq!(eval("-6"), Ok(NumberVal(-6.0)));
        assert_eq!(eval("! true"), Ok(BoolVal(false)));
        assert_eq!(eval("! false"), Ok(BoolVal(true)));
        assert_eq!(eval("! 1"), Ok(BoolVal(false)));
        assert_eq!(eval("! 0"), Ok(BoolVal(false)));
        assert_eq!(eval("! \"\""), Ok(BoolVal(false)));
        assert_eq!(eval("! nil"), Ok(BoolVal(true)));
    }

    #[test]
    fn test_interpret_literals() {
        assert_eq!(interpret("42;"), Ok(NumberVal(42.0)));
        assert_eq!(interpret("nil;"), Ok(NilVal));
    }

    #[test]
    fn test_interpret_operators() {
        assert_eq!(interpret("40 + 2;"), Ok(NumberVal(42.0)));
        assert_eq!(interpret("\"foo\" + \"bar\";"), Ok(StringVal(Rc::new("foobar".into()))));
    }

    #[test]
    fn test_interpret_string_plus_number_coerces() {
        // https://www.craftinginterpreters.com/evaluating-expressions.html#challenges
        assert_eq!(interpret("\"scone\" + 4;"), Ok(StringVal(Rc::new("scone4".into()))));
        assert_eq!(interpret("4 + \"scone\";"), Ok(StringVal(Rc::new("4scone".into()))));
        assert_eq!(interpret("\"scone\" + true;"), Ok(StringVal(Rc::new("sconetrue".into()))));
        assert_eq!(interpret("true + \"scone\";"), Ok(StringVal(Rc::new("truescone".into()))));
        assert_eq!(interpret("\"scone\" + nil;"), Ok(StringVal(Rc::new("sconenil".into()))));
        assert_eq!(interpret("nil + \"scone\";"), Ok(StringVal(Rc::new("nilscone".into()))));
    }

    #[test]
    fn test_interpret_print() {
        assert_eq!(interpret("print \"print test\";"), Ok(NilVal));
    }

    #[test]
    fn test_interpret_var() {
        assert_eq!(interpret("var x;"), Ok(NilVal));
        assert_eq!(interpret("var x = 1;"), Ok(NilVal));
        assert_eq!(interpret("var x = x;"), Err(RuntimeError::new(SourceLoc::new(1, 9), "parse error: Cannot read local variable in its own initializer: x")));
    }

    #[test]
    fn test_interpret_unicode_var_name() {
        assert_eq!(interpret("var λ = 2; λ;"), Ok(NumberVal(2.0)));
    }

    #[test]
    fn test_interpret_var_use() {
        assert_eq!(interpret("var x = 1; x;"), Ok(NumberVal(1.0)));
        assert_eq!(interpret("var x = 1; var y = 3; x = y = 5; x;"), Ok(NumberVal(5.0)));
        assert_eq!(interpret("x;"), Err(RuntimeError::new(SourceLoc::new(1, 1), "Undefined variable 'x'.")));
        assert_eq!(interpret("var x = 1; y;"), Err(RuntimeError::new(SourceLoc::new(1, 12), "Undefined variable 'y'.")));
    }

    #[test]
    fn test_interpret_var_assign() {
        assert_eq!(interpret("var x = 1; x = 2; x;"), Ok(NumberVal(2.0)));
        assert_eq!(interpret("x = 1;"), Err(RuntimeError::new(SourceLoc::new(1, 3), "Undefined variable 'x'.")));
    }

    #[test]
    fn test_interpret_blocks() {
        assert_eq!(interpret("var x = 1; { var x = 2; x; }"), Ok(NumberVal(2.0)));
        assert_eq!(interpret("var x = 1; { var x = 2; } x;"), Ok(NumberVal(1.0)));
        // Assignment.
        assert_eq!(interpret("var x = 1; { var x = 2; x = 3; x; }"), Ok(NumberVal(3.0)));
        assert_eq!(interpret("var x = 1; { var x = 2; x = 3; } x;"), Ok(NumberVal(1.0)));
    }

    #[test]
    fn test_interpret_repl_line() {
        assert_eq!(interpret_repl_line("1 + 2"), Ok(NumberVal(3.0)));
        assert_eq!(interpret_repl_line("1 + 2;"), Ok(NumberVal(3.0)));
        assert_eq!(interpret_repl_line("1 + 2; 10"), Ok(NumberVal(10.0)));
    }

    #[test]
    fn test_interpret_if() {
        assert_eq!(interpret("var x = 1; if (0 < 1) x = 2; else x = 3; x;"), Ok(NumberVal(2.0)));
        assert_eq!(interpret("var x = 1; if (0 > 1) x = 2; else x = 3; x;"), Ok(NumberVal(3.0)));
        assert_eq!(interpret("var x = 1; if (0 > 1) { x = 2; } else { x = 3; } x;"), Ok(NumberVal(3.0)));
        // Dangling else ambiguity.
        assert_eq!(interpret("var x = 1; if (true) if (false) x = 2; else x = 3; x;"), Ok(NumberVal(3.0)));
    }

    #[test]
    fn test_interpret_and_or() {
        // TODO: test short-circuiting.
        assert_eq!(interpret("1 and 2;"), Ok(NumberVal(2.0)));
        assert_eq!(interpret("false and 2;"), Ok(BoolVal(false)));
        assert_eq!(interpret("1 or 2;"), Ok(NumberVal(1.0)));
        assert_eq!(interpret("nil or 2;"), Ok(NumberVal(2.0)));
    }

    #[test]
    fn test_interpret_while() {
        assert_eq!(interpret("var x = 0; while (x < 3) x = x + 1; x;"), Ok(NumberVal(3.0)));
    }

    #[test]
    fn test_interpret_while_break() {
        assert_eq!(interpret("var x = 0; while (true) { if (x > 3) break; x = x + 1; } x;"), Ok(NumberVal(4.0)));
    }

    #[test]
    fn test_interpret_top_level_break() {
        assert_eq!(interpret("1 + 2;\nbreak;"), Err(RuntimeError::new(SourceLoc::new(2, 1), "parse error: Found break statement outside of loop body")));
    }

    #[test]
    fn test_interpret_for_loop() {
        assert_eq!(interpret("var x = 1;\nfor (var i = 0; i < 3; i = i + 1)\nx = x * 2;\nx;"), Ok(NumberVal(8.0)));
        assert_eq!(interpret("var x = 1;\nfor (var i = 0; i < 3; i = i + 1)\n{ x = x * 2; }\nx;"), Ok(NumberVal(8.0)));
    }

    #[test]
    fn test_interpret_for_loop_break() {
        assert_eq!(interpret("
            var x = 1;
            for (;; x = x + 1) {
                if (x > 3) break;
            }
            x;"), Ok(NumberVal(4.0)));
    }

    #[test]
    fn test_interpret_native_function_call() {
        assert_eq!(interpret("
            var t = clock();
            t > 0;"), Ok(BoolVal(true)));
    }

    #[test]
    fn test_interpret_function_call() {
        assert_eq!(interpret("
            fun succ(x) {
                return x + 1;
            }
            succ(1);"), Ok(NumberVal(2.0)));
    }

    #[test]
    fn test_interpret_no_implicit_return_value() {
        assert_eq!(interpret("
            fun do(x) {
                x + 1;
            }
            do(1);"), Ok(NilVal));
    }

    #[test]
    fn test_interpret_scope_resolved() {
        assert_eq!(interpret("
            var x = \"global\";
            {
                fun show() {
                    return x;
                }
                show();
                var x = \"local\";
                show();
            }"), Ok(StringVal(Rc::new("global".to_string()))));
    }

    #[test]
    fn test_interpret_use_global_variable_when_not_resolvable() {
        // See https://www.craftinginterpreters.com/global-variables.html
        assert_eq!(interpret("
            fun showVariable() {
                return global;
            }

            var global = \"after\";
            showVariable();
            "), Ok(StringVal(Rc::new("after".to_string()))));
    }

    #[test]
    fn test_interpret_use_global_variable_when_not_resolvable_and_not_defined() {
        // See https://www.craftinginterpreters.com/global-variables.html
        assert_eq!(interpret("
            fun showVariable() {
                return global;
            }

            showVariable();
            "), Err(RuntimeError::new(SourceLoc::new(3, 24), "Undefined variable 'global'.")));
    }

    #[test]
    fn test_interpret_assign_to_global_variable_when_not_resolvable() {
        // See https://www.craftinginterpreters.com/global-variables.html
        assert_eq!(interpret("
            fun changeVariable() {
                global = \"changed\";
            }

            var global = \"original\";
            changeVariable();
            global;
            "), Ok(StringVal(Rc::new("changed".to_string()))));
    }

    #[test]
    fn test_interpret_top_level_return() {
        assert_eq!(interpret("1 + 2;\nreturn;"), Err(RuntimeError::new(SourceLoc::new(2, 1), "parse error: Cannot return from top-level code.")));
    }

    #[test]
    fn test_instance_fields_set_get() {
        assert_eq!(interpret("
            class Point {
            }
            var p = Point();
            p.x = 1;
            p.y = 2;
            p.x + p.y;"), Ok(NumberVal(3.0)));
    }

    #[test]
    fn test_instance_get_undefined_field() {
        assert_eq!(interpret("
            class Point {
            }
            var p = Point();
            p.x;"), Err(RuntimeError::new(SourceLoc::new(5, 14), "Undefined property 'x'.")));
    }

    #[test]
    fn test_instance_method_call() {
        assert_eq!(interpret("
            class Computer {
                answer() {
                    return 42;
                }
            }
            var c = Computer();
            c.answer();"), Ok(NumberVal(42.0)));
    }

    #[test]
    fn test_instance_access_field_with_this() {
        assert_eq!(interpret("
            class Box {
                result() {
                    return this.value;
                }
            }
            var b = Box();
            b.value = 42;
            b.result();"), Ok(NumberVal(42.0)));
    }

    #[test]
    fn test_this_outside_method_body() {
        assert_eq!(interpret("print this;"), Err(RuntimeError::new(SourceLoc::new(1, 7), "parse error: Cannot use \"this\" outside of method body")));
        assert_eq!(interpret("fun foo() { return this; }"), Err(RuntimeError::new(SourceLoc::new(1, 20), "parse error: Cannot use \"this\" outside of method body")));
    }

    #[test]
    fn test_class_constructor() {
        assert_eq!(interpret("
            class Point {
                init() {
                    this.x = 0;
                    this.y = 2;
                }
            }
            var p = Point();
            p.y;"), Ok(NumberVal(2.0)));
    }

    #[test]
    fn test_class_constructor_with_one_parameter() {
        assert_eq!(interpret("
            class Box {
                init(value) {
                    this.value = value;
                }
            }
            var box = Box(42);
            box.value;"), Ok(NumberVal(42.0)));
    }

    #[test]
    fn test_class_constructor_with_parameters() {
        assert_eq!(interpret("
            class Point {
                init(x, y) {
                    this.x = x;
                    this.y = y;
                }
            }
            var p = Point(5, 10);
            p.x;"), Ok(NumberVal(5.0)));
    }

    #[test]
    fn test_class_constructor_checks_zero_arity_when_no_init_defined() {
        assert_eq!(interpret("
            class Box {
            }
            Box(1000000);"), Err(RuntimeError::new(SourceLoc::new(4, 16), "Expected 0 arguments but got 1.")));
    }

    #[test]
    fn test_class_constructor_checks_non_zero_arity() {
        assert_eq!(interpret("
            class Point {
                init(x, y) {
                    this.x = x;
                    this.y = y;
                }
            }
            Point(5, 10, 1000000);"), Err(RuntimeError::new(SourceLoc::new(8, 18), "Expected 2 arguments but got 3.")));
    }

    #[test]
    fn test_class_constructor_disallows_return_with_expression() {
        assert_eq!(interpret("
            class Box {
                init() {
                    return 42;
                }
            }"), Err(RuntimeError::new(SourceLoc::new(4, 21), "parse error: Cannot return a value from an initializer.")));
    }

    #[test]
    fn test_class_constructor_allows_return_with_no_expression() {
        assert_eq!(interpret("
            class Box {
                init() {
                    return;
                }
            }
            nil;"), Ok(NilVal));
    }

    #[test]
    fn test_static_class_method() {
        assert_eq!(interpret("
            class Math {
                class square(x) {
                    return x * x;
                }
            }
            Math.square(2);"), Ok(NumberVal(4.0)));
    }

    #[test]
    fn test_static_class_method_can_access_this() {
        assert_eq!(interpret("
            class Math {
                class square(x) {
                    return this.impl(x);
                }
                class impl(x) {
                    return x * x;
                }
            }
            Math.square(2);"), Ok(NumberVal(4.0)));
    }

    #[test]
    fn test_static_class_method_can_access_super() {
        assert_eq!(interpret("
            class Impl {
                class impl(x) {
                    return x * x;
                }
            }
            class Math < Impl {
                class square(x) {
                    return super.impl(x);
                }
            }
            Math.square(2);"), Ok(NumberVal(4.0)));
    }

    #[test]
    fn test_static_class_method_is_not_accessible_from_instance() {
        assert_eq!(interpret("
            class Math {
                class square(x) {
                    return x * x;
                }
            }
            var m = Math();
            m.square(2);"), Err(RuntimeError::new(SourceLoc::new(8, 14), "Undefined property 'square'.")));
    }


    #[test]
    fn test_class_with_superclass_can_call_super_methods() {
        assert_eq!(interpret("
            class Point2 {
                first() {
                    return this.x;
                }
            }
            class Point3 < Point2 {
            }
            var p = Point3();
            p.x = 2;
            p.first();"), Ok(NumberVal(2.0)));
    }

    #[test]
    fn test_class_calling_super_from_subclass_method() {
        assert_eq!(interpret("
            class Point2 {
                first() {
                    return this.x;
                }
            }
            class Point3 < Point2 {
                second() {
                    return super.first() + 1;
                }
            }
            var p = Point3();
            p.x = 2;
            p.second();"), Ok(NumberVal(3.0)));
    }

    #[test]
    fn test_class_inheriting_from_itself() {
        assert_eq!(interpret("
            class Box < Box {}
            "), Err(RuntimeError::new(SourceLoc::new(2, 25), "parse error: A class cannot inherit from itself.")));
    }

    #[test]
    fn test_class_inheriting_from_non_identifier() {
        // In the future, we could make arbitrary expressions work.
        assert_eq!(interpret("
            class Box < 2 {}
            "), Err(RuntimeError::new(SourceLoc::new(2, 25), "parse error: Expected identifier after \"<\" in class declaration")));
    }

    #[test]
    fn test_class_inheriting_from_non_class() {
        assert_eq!(interpret("
            var x = \"not a class\";
            class Box < x {}
            "), Err(RuntimeError::new_with_details(SourceLoc::new(3, 19), "Superclass must be a class.", "Superclass must be a class; instead found: \"not a class\"")));
    }

    #[test]
    fn test_super_outside_method_body() {
        assert_eq!(interpret("super.x;"), Err(RuntimeError::new(SourceLoc::new(1, 1), "parse error: Cannot use \"super\" outside of a class")));
        assert_eq!(interpret("fun foo() { super.x; }"), Err(RuntimeError::new(SourceLoc::new(1, 13), "parse error: Cannot use \"super\" outside of a class")));
        assert_eq!(interpret("
            class Box {}
            fun foo() { super.x; }
            "), Err(RuntimeError::new(SourceLoc::new(3, 25), "parse error: Cannot use \"super\" outside of a class")));
    }

    #[test]
    fn test_super_outside_subclass_method() {
        assert_eq!(interpret("
            class Box {
                foo() {
                    super.x;
                }
            }"), Err(RuntimeError::new(SourceLoc::new(4, 21), "parse error: Cannot use \"super\" in class without a superclass")));
    }
}
