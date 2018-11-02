use ast::*;
use value::*;

pub struct Interpreter {
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<Value, RuntimeError> {
        let mut result = Value::NilVal;
        for stmt in statements.iter() {
            result = self.execute(stmt)?;
        }

        Ok(result)
    }

    pub fn execute(&mut self, statement: &Stmt) -> Result<Value, RuntimeError> {
        match statement {
            Stmt::Expression(expr) => self.evaluate(expr),
            Stmt::Print(expr) => {
                let value = self.evaluate(expr)?;
                println!("{}", value.to_runtime_string());

                Ok(Value::NilVal)
            }
        }
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        use value::Value::*;
        match expr {
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
            Expr::Grouping(e) => self.evaluate(e),
            Expr::LiteralBool(b) => Ok(BoolVal(*b)),
            Expr::LiteralNil => Ok(NilVal),
            Expr::LiteralNumber(x) => Ok(NumberVal(*x)),
            Expr::LiteralString(s) => Ok(StringVal(s.clone())),
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use value::Value::*;
    use util::parse;
    use util::parse_expression;

    fn interpret(code: &str) -> Result<Value, RuntimeError> {
        let mut interpreter = Interpreter::new();

        interpreter.interpret(parse(code))
    }

    fn eval(code: &str) -> Result<Value, RuntimeError> {
        let mut interpreter = Interpreter::new();

        interpreter.evaluate(&parse_expression(code))
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
}
