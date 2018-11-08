use std::mem;
use std::time::SystemTime;

use ast::*;
use environment::*;
use source_loc::*;
use value::*;

pub struct Interpreter {
    env: Box<Environment>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut globals = Environment::default();
        globals.define(&NativeFunctionId::Clock.to_string(),
            Value::NativeFunctionVal(NativeFunctionId::Clock));

        Interpreter {
            env: Box::new(globals),
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
                let mut env = Box::new(Environment::new(None));
                let enclosing = mem::replace(&mut self.env, env);
                self.env.enclosing = Some(enclosing);

                // Execute statements.
                let mut result = Ok(Value::NilVal);
                for statement in statements {
                    result = self.exec(statement);
                    if result.is_err() {
                        break;
                    }
                }

                // Restore previous environment.
                let enclosing = mem::replace(&mut self.env.enclosing, None)
                                .expect("interpreter::execute: enclosing should never be empty here");
                self.env = enclosing;

                result
            }
            Stmt::Break(loc) => Err(ExecutionInterrupt::Break(*loc)),
            Stmt::Expression(expr) => self.evaluate(expr).map_err(|err| err.into()),
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
            Stmt::Var(identifier, expr) => {
                let value = self.evaluate(expr)?;
                self.env.define(identifier, value);

                Ok(Value::NilVal)
            }
            Stmt::While(condition, body) => {
                while self.evaluate(condition)?.is_truthy() {
                    match self.exec(body) {
                        Ok(_) => (),
                        Err(ExecutionInterrupt::Break(_)) => break,
                        Err(ExecutionInterrupt::Error(error)) => {
                            return Err(error.into());
                        }
                    }
                }

                Ok(Value::NilVal)
            }
        }
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        use value::Value::*;
        match expr {
            Expr::Assign(id, dist_cell, expr, loc) => {
                let value = self.evaluate(expr)?;
                self.env.assign_at(&id, dist_cell.get(), value.clone())
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
                                Ok(StringVal(format!("{}{}", s1, s2)))
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
                for (i, arg) in arguments.iter().enumerate() {
                    arg_vals[i] = self.evaluate(arg)?;
                }

                self.call(callee_val, arg_vals, *loc)
            }
            Expr::Grouping(e) => self.evaluate(e),
            Expr::LiteralBool(b) => Ok(BoolVal(*b)),
            Expr::LiteralNil => Ok(NilVal),
            Expr::LiteralNumber(x) => Ok(NumberVal(*x)),
            Expr::LiteralString(s) => Ok(StringVal(s.clone())),
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
                self.env.get_at(id, dist_cell.get())
                    // TODO: Don't clone here since it copies strings.  We only
                    // have a reference to the value, though.
                    .map(|value| value.clone())
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

    fn call(&mut self, callee: Value, args: Vec<Value>, loc: SourceLoc)
        -> Result<Value, RuntimeError>
    {
        match callee {
            Value::NativeFunctionVal(id) => {
                let native_function = NativeFunction::new(id);
                let arity = native_function.arity();
                let args_len = args.len();
                if args_len != arity {
                    return Err(RuntimeError::new(loc, &format!("Function expected {} arguments, but you attempted to call it with {}", arity, args_len)));
                }
                let return_value_result = native_function.call(args, loc);

                return_value_result.map_err(|e| e.into())
            }
            _ => Err(RuntimeError::new(loc, &format!("You can only call functions and classes, but you tried to call: {}", callee))),
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
                let sys_time = SystemTime::now();

                match sys_time.duration_since(SystemTime::UNIX_EPOCH) {
                    Ok(duration) => Ok(Value::NumberVal(duration.as_secs() as f64)),
                    Err(_) => Err(RuntimeError::new(loc, "Unable to get system time since now is before the epoch")),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;
    use resolver;
    use value::Value::*;

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
        assert_eq!(eval("\"hello\""), Ok(StringVal("hello".to_string())));
        assert_eq!(eval("true"), Ok(BoolVal(true)));
        assert_eq!(eval("false"), Ok(BoolVal(false)));
        assert_eq!(eval("nil"), Ok(NilVal));
    }

    #[test]
    fn test_eval_binary_ops() {
        assert_eq!(eval("40 + 2"), Ok(NumberVal(42.0)));
        assert_eq!(eval("\"foo\" + \"bar\""), Ok(StringVal("foobar".to_string())));
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
        assert_eq!(interpret("\"foo\" + \"bar\";"), Ok(StringVal("foobar".into())));
    }

    #[test]
    fn test_interpret_print() {
        assert_eq!(interpret("print \"print test\";"), Ok(NilVal));
    }

    #[test]
    fn test_interpret_var() {
        assert_eq!(interpret("var x;"), Ok(NilVal));
        assert_eq!(interpret("var x = 1;"), Ok(NilVal));
    }

    #[test]
    fn test_interpret_var_use() {
        assert_eq!(interpret("var x = 1; x;"), Ok(NumberVal(1.0)));
        assert_eq!(interpret("var x = 1; var y = 3; x = y = 5; x;"), Ok(NumberVal(5.0)));
        assert_eq!(interpret("x;"), Err(RuntimeError::new(SourceLoc::new(1), "Undefined variable: x")));
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
}
