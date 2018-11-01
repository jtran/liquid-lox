use expr::*;
use value::*;

pub struct Interpreter {
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        use value::Value::*;
        match expr {
            Expr::Binary(left, op, right) => {
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
                            (StringVal(_), _) => Err(RuntimeError::new(&format!("expected string but found {} evaluating plus in expression: {:?}", right_type, expr))),
                            (NumberVal(_), _) => Err(RuntimeError::new(&format!("expected number but found {} evaluating plus in expression: {:?}", right_type, expr))),
                            _ => Err(RuntimeError::new(&format!("expected number or string but found {} evaluating plus in expression: {:?}", left_type, expr))),
                        }
                    },
                    BinaryOperator::Minus => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 - x2)),
                            _ => Err(RuntimeError::new(&format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::Multiply => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 * x2)),
                            _ => Err(RuntimeError::new(&format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::Divide => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(NumberVal(x1 / x2)),
                            _ => Err(RuntimeError::new(&format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    // Comparison operators.
                    BinaryOperator::Equal => Ok(BoolVal(left_val.is_equal(&right_val))),
                    BinaryOperator::NotEqual => Ok(BoolVal(!left_val.is_equal(&right_val))),
                    BinaryOperator::Less => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 < x2)),
                            _ => Err(RuntimeError::new(&format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::LessEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 <= x2)),
                            _ => Err(RuntimeError::new(&format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::Greater => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 > x2)),
                            _ => Err(RuntimeError::new(&format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                    BinaryOperator::GreaterEqual => {
                        match (left_val, right_val) {
                            (NumberVal(x1), NumberVal(x2)) => Ok(BoolVal(x1 >= x2)),
                            _ => Err(RuntimeError::new(&format!("expected numbers but found {} and {} evaluating expression: {:?}", left_type, right_type, expr))),
                        }
                    },
                }
            }
            Expr::Grouping(e) => self.evaluate(e),
            Expr::LiteralBool(b) => Ok(BoolVal(*b)),
            Expr::LiteralNil => Ok(NilVal),
            Expr::LiteralNumber(x) => Ok(NumberVal(*x)),
            Expr::LiteralString(s) => Ok(StringVal(s.clone())),
            Expr::Unary(op, e) => {
                let v = self.evaluate(e)?;
                let right_type = v.runtime_type();

                match op {
                    UnaryOperator::Minus => {
                        match v {
                            NumberVal(x) => Ok(NumberVal(-x)),
                            _ => Err(RuntimeError::new(&format!("expected number but found {} evaluating expression: {:?}", right_type, expr))),
                        }
                    },
                    UnaryOperator::Not => Ok(BoolVal(!v.is_truthy())),
                }
            }
        }
    }
}
