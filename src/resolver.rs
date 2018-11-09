use std::collections::HashMap;

use ast::*;
use parser::ParseErrorCause;

pub fn resolve(statements: &mut Vec<Stmt>) -> Result<(), ParseErrorCause> {
    let mut resolver = Resolver::new();
    resolver.resolve(statements)?;

    Ok(())
}

#[allow(dead_code)]
pub fn resolve_expression(expression: &mut Expr) -> Result<(), ParseErrorCause> {
    let mut resolver = Resolver::new();
    resolver.resolve_expression(expression)?;

    Ok(())
}

// Resolves uses of identifiers to their scope.  We decide statically which var
// declaration each reference refers to.
#[derive(Clone, Debug)]
pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    function_type: FunctionType,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum FunctionType {
    NoFunction,
    Plain,
}

impl Resolver {
    pub fn new() -> Resolver {
        let mut resolver = Resolver {
            scopes: Vec::with_capacity(1),
            function_type: FunctionType::NoFunction,
        };

        // Start with the global scope.
        resolver.begin_scope();

        resolver
    }

    pub fn resolve(&mut self, statements: &mut Vec<Stmt>) -> Result<(), ParseErrorCause> {
        for statement in statements.iter_mut() {
            self.resolve_statement(statement)?;
        }

        Ok(())
    }

    pub fn resolve_statement(&mut self, statement: &mut Stmt) -> Result<(), ParseErrorCause> {
        match statement {
            Stmt::Block(statements) => {
                self.begin_scope();

                let mut result = Ok(());
                for statement in statements.iter_mut() {
                    result = self.resolve_statement(statement);
                    if result.is_err() {
                        break;
                    }
                }

                self.end_scope();

                result
            }
            Stmt::Break(_) => Ok(()),
            Stmt::Expression(expr) => self.resolve_expression(expr),
            Stmt::Fun(fun_def) => {
                self.define(&fun_def.name);

                // Track that we're in a function.
                let enclosing_func_type = self.function_type;
                self.function_type = FunctionType::Plain;

                self.begin_scope();
                for parameter in &fun_def.parameters {
                    self.define(&parameter.name);
                }
                let mut result = Ok(());
                for mut body_statement in &mut fun_def.body {
                    result = self.resolve_statement(&mut body_statement);
                    if result.is_err() {
                        break;
                    }
                }
                self.end_scope();

                // Restore previous function type.
                self.function_type = enclosing_func_type;

                result
            }
            Stmt::If(condition, then_stmt, else_stmt_opt) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(then_stmt)?;
                if let Some(else_stmt) = else_stmt_opt {
                    self.resolve_statement(else_stmt)?;
                }

                Ok(())
            }
            Stmt::Print(expr) => self.resolve_expression(expr),
            Stmt::Return(expr, loc) => {
                if self.function_type != FunctionType::Plain {
                    return Err(ParseErrorCause::new(*loc, "Found return statement outside of function body"));
                }

                self.resolve_expression(expr)
            }
            Stmt::Var(name, expr) => {
                self.declare(name);
                self.resolve_expression(expr)?;
                self.define(name);

                Ok(())
            }
            Stmt::While(condition, body) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;

                Ok(())
            }
        }
    }

    pub fn resolve_expression(&mut self, expression: &mut Expr) -> Result<(), ParseErrorCause> {
        match expression {
            Expr::Assign(identifier, dist_cell, expr, _) => {
                self.resolve_expression(expr)?;
                let distance = self.resolve_local_variable(identifier);
                dist_cell.set(distance);

                Ok(())
            }
            Expr::Binary(left, _, right, _) => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;

                Ok(())
            }
            Expr::Call(callee, arguments, _) => {
                self.resolve_expression(callee)?;
                for argument in arguments.iter_mut() {
                    self.resolve_expression(argument)?;
                }

                Ok(())
            }
            Expr::Grouping(expr) => self.resolve_expression(expr),
            Expr::LiteralBool(_) => Ok(()),
            Expr::LiteralNil => Ok(()),
            Expr::LiteralNumber(_) => Ok(()),
            Expr::LiteralString(_) => Ok(()),
            Expr::Logical(left, _, right) => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;

                Ok(())
            }
            Expr::Unary(_, right, _) => {
                self.resolve_expression(right)?;

                Ok(())
            }
            Expr::Variable(identifier, dist_cell, loc) => {
                // Scope to drop the borrow of self.
                {
                    let scope = self.scopes.last().expect("Resolver::resolve_expression: last scope to be present");
                    match scope.get(identifier) {
                        None => (),
                        Some(true) => (),
                        Some(false) => return Err(ParseErrorCause::new(*loc, &format!("Cannot read local variable in its own initializer: {}", identifier))),
                    };
                }

                let distance = self.resolve_local_variable(identifier);
                dist_cell.set(distance);

                Ok(())
            }
        }
    }

    fn resolve_local_variable(&mut self, identifier: &str) -> usize {
        let len = self.scopes.len();
        if len == 0 {
            return 0;
        }

        let mut i = len;
        while i > 0 {
            i -= 1;

            let scope = &self.scopes[i];
            if scope.contains_key(identifier) {
                let distance = len - 1 - i;

                return distance;
            }
        }

        0
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, identifier: &str) {
        let scope = self.scopes.last_mut().expect("Resolver::declare: I'm trying to look up the most-local scope, but there are none");
        scope.insert(identifier.to_string(), false);
    }

    fn define(&mut self, identifier: &str) {
        let scope = self.scopes.last_mut().expect("Resolver::define: I'm trying to look up the most-local scope, but there are none");
        scope.insert(identifier.to_string(), true);
    }
}
