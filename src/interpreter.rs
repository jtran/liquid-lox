use std::cell::RefCell;
use std::mem;
use std::ops::Deref;
use std::rc::Rc;
use std::time::SystemTime;

use crate::ast::*;
use crate::environment::*;
use crate::source_loc::*;
use crate::value::*;

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut prelude = Environment::default();
        for native_id in all_native_ids() {
            prelude.define(&native_id.to_string(),
                           Value::NativeFunctionVal(native_id));
        }
        // We need an empty scope so that resolved indexes can start from 0.
        //
        // Note: this behavior must match the Resolver.
        let top_scope = Environment::new(Some(Rc::new(RefCell::new(prelude))));

        Interpreter {
            env: Rc::new(RefCell::new(top_scope)),
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
                let new_env = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(&self.env)))));

                self.exec_in_env(statements.as_slice(), new_env)
            }
            Stmt::Break(loc) => Err(ExecutionInterrupt::Break(*loc)),
            Stmt::Expression(expr) => self.evaluate(expr).map_err(|err| err.into()),
            Stmt::Fun(fun_def) => {
                let closure = Value::ClosureVal(Rc::new(fun_def.clone()), Rc::clone(&self.env));
                let mut env = self.env.deref().borrow_mut();
                env.define(&fun_def.name, closure);

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
                        RuntimeError::new(*loc, &format!("Undefined variable: {}", &id))
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
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 + x2)),
                            (StringVal(s1), StringVal(s2)) => {
                                Ok(StringVal(Rc::new(format!("{}{}", s1.deref(), s2.deref()))))
                            }
                            (StringVal(_), _) => Err(RuntimeError::new(*loc, &format!("expected string but found {} evaluating plus in expression: {:?}", right_type, expr))),
                            (NumberVal(_), _) => Err(RuntimeError::new(*loc, &format!("expected number but found {} evaluating plus in expression: {:?}", right_type, expr))),
                            _ => Err(RuntimeError::new(*loc, &format!("expected number or string but found {} evaluating plus in expression: {:?}", left_type, expr))),
                        }
                    },
                    BinaryOperator::Minus => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 - x2)),
                            _ => Err(RuntimeError::new(*loc, &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::Multiply => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 * x2)),
                            _ => Err(RuntimeError::new(*loc, &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
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
                            _ => Err(RuntimeError::new(*loc, &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    // Comparison operators.
                    BinaryOperator::Equal => Ok(BoolVal(left_val.is_equal(&right_val))),
                    BinaryOperator::NotEqual => Ok(BoolVal(!left_val.is_equal(&right_val))),
                    BinaryOperator::Less => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 < x2)),
                            _ => Err(RuntimeError::new(*loc, &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::LessEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 <= x2)),
                            _ => Err(RuntimeError::new(*loc, &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::Greater => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 > x2)),
                            _ => Err(RuntimeError::new(*loc, &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::GreaterEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 >= x2)),
                            _ => Err(RuntimeError::new(*loc, &format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
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
            Expr::Variable(id, dist_cell, loc) => {
                let env = self.env.deref().borrow();

                env.get_at(id, dist_cell.get())
                    // In the case that the variable is not in the environment,
                    // generate a runtime error.
                    .ok_or_else(|| RuntimeError::new(*loc, &format!("Undefined variable: {}", id)))
            }
            Expr::Unary(op, e, loc) => {
                let v = self.evaluate(e)?;
                let right_type = v.runtime_type();

                match op {
                    UnaryOperator::Minus => {
                        match v {
                            NumberVal(x) => Ok(NumberVal(-x)),
                            _ => Err(RuntimeError::new(*loc, &format!("expected number but found {} evaluating expression: {:?}", right_type, expr))),
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

                let return_value_result = native_function.call(args, loc);

                return_value_result
            }
            Value::ClosureVal(fun_def, env_sptr) => {
                self.check_call_arity(fun_def.parameters.len(), args.len(), &fun_def.source_loc)?;
                // Create a new environment, enclosed by closure's environment.
                let mut new_env = Environment::new(Some(Rc::clone(&env_sptr)));
                // Bind parameters to argument values.
                for (parameter, arg) in fun_def.parameters.iter().zip(args) {
                    new_env.define(&parameter.name, arg);
                }
                // Execute function body in new environment.
                let new_env_sptr = Rc::new(RefCell::new(new_env));
                let return_value_result = self.exec_in_env(fun_def.body.as_slice(), new_env_sptr);

                self.unwrap_return_value(return_value_result)
            }
            _ => Err(RuntimeError::new(loc, &format!("You can only call functions and classes, but you tried to call: {}", callee))),
        }
    }

    fn check_call_arity(&self, arity: usize, num_args: usize, loc: &SourceLoc)
        -> Result<(), RuntimeError>
    {
        if num_args != arity {
            return Err(RuntimeError::new(*loc, &format!("Function called with wrong number of arguments; expected {} but given {}", arity, num_args)));
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
        let loc = SourceLoc::new(1);
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
    fn test_interpret_print() {
        assert_eq!(interpret("print \"print test\";"), Ok(NilVal));
    }

    #[test]
    fn test_interpret_var() {
        assert_eq!(interpret("var x;"), Ok(NilVal));
        assert_eq!(interpret("var x = 1;"), Ok(NilVal));
        assert_eq!(interpret("var x = x;"), Err(RuntimeError::new(SourceLoc::new(1), "parse error: Cannot read local variable in its own initializer: x")));
    }

    #[test]
    fn test_interpret_var_use() {
        assert_eq!(interpret("var x = 1; x;"), Ok(NumberVal(1.0)));
        assert_eq!(interpret("var x = 1; var y = 3; x = y = 5; x;"), Ok(NumberVal(5.0)));
        assert_eq!(interpret("x;"), Err(RuntimeError::new(SourceLoc::new(1), "Undefined variable: x")));
        assert_eq!(interpret("var x = 1; y;"), Err(RuntimeError::new(SourceLoc::new(1), "Undefined variable: y")));
    }

    #[test]
    fn test_interpret_var_assign() {
        assert_eq!(interpret("var x = 1; x = 2; x;"), Ok(NumberVal(2.0)));
        assert_eq!(interpret("x = 1;"), Err(RuntimeError::new(SourceLoc::new(1), "Undefined variable: x")));
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
        assert_eq!(interpret("1 + 2;\nbreak;"), Err(RuntimeError::new(SourceLoc::new(2), "parse error: Found break statement outside of loop body")));
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
    fn test_interpret_top_level_return() {
        assert_eq!(interpret("1 + 2;\nreturn;"), Err(RuntimeError::new(SourceLoc::new(2), "parse error: Found return statement outside of function body")));
    }
}
