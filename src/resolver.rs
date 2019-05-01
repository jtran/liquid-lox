use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::ast::*;
use crate::environment::*;
use crate::parser::ParseErrorCause;
use crate::value::*;

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
    scopes: Vec<HashMap<String, VarResolveState>>,
    function_type: FunctionType,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VarResolveState {
    pub frame_index: usize,
    pub is_defined: bool,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum FunctionType {
    NoFunction,
    Plain,
    Initializer,
    Method,
}

impl Resolver {
    pub fn new() -> Resolver {
        let mut resolver = Resolver {
            scopes: Vec::with_capacity(1),
            function_type: FunctionType::NoFunction,
        };

        // Start with the prelude scope.
        resolver.begin_scope();
        for native_id in all_native_ids() {
            resolver.define(&native_id.to_string())
        }

        // Create a top-level scope.
        //
        // Note: this behavior must match the Interpreter.
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
            Stmt::Class(class_def) => {
                self.define(&class_def.name);

                self.begin_scope();
                self.define("this");

                let mut result = Ok(());
                for method in class_def.methods.iter_mut() {
                    let fun_type = if method.name == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };

                    result = self.resolve_function(method, fun_type);
                    if result.is_err() {
                        break;
                    }
                }

                self.end_scope();

                result
            }
            Stmt::Expression(expr) => self.resolve_expression(expr),
            Stmt::Fun(fun_def) => {
                self.define(&fun_def.name);

                self.resolve_function(fun_def, FunctionType::Plain)
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
                match self.function_type {
                    FunctionType::Plain
                    | FunctionType::Initializer
                    | FunctionType::Method => (),
                    _ => return Err(ParseErrorCause::new(*loc, "Found return statement outside of function body")),
                }

                // TODO: This currently doesn't distinguish between returning
                // nil and no return value.
                if self.function_type == FunctionType::Initializer {
                    match expr {
                        Expr::LiteralNil => (),
                        _ => {
                            return Err(ParseErrorCause::new(*loc, "Cannot return a value from a class's initializer"));
                        }
                    }
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
                let var_loc = self.resolve_local_variable(identifier);
                dist_cell.set(var_loc);

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
            Expr::Get(expr, _, _) => self.resolve_expression(expr),
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
            Expr::Set(object_expr, _, value_expr, _) => {
                self.resolve_expression(value_expr)?;
                self.resolve_expression(object_expr)?;

                Ok(())
            }
            Expr::Unary(_, right, _) => {
                self.resolve_expression(right)?;

                Ok(())
            }
            Expr::Variable(identifier, dist_cell, loc) => {
                match self.function_type {
                    FunctionType::Initializer | FunctionType::Method => (),
                    _ => {
                        if identifier == "this" {
                            return Err(ParseErrorCause::new(*loc, "Cannot use \"this\" outside of method body"));
                        }
                    }
                }

                // Scope to drop the borrow of self.
                {
                    let scope = self.scopes.last().expect("Resolver::resolve_expression: last scope to be present");
                    match scope.get(identifier) {
                        None => (),
                        Some(ref resolve_state) => {
                            if ! resolve_state.is_defined {
                                return Err(ParseErrorCause::new(*loc, &format!("Cannot read local variable in its own initializer: {}", identifier)));
                            }
                        }
                    };
                }

                let var_loc = self.resolve_local_variable(identifier);
                dist_cell.set(var_loc);

                Ok(())
            }
        }
    }

    fn resolve_function(&mut self,
                        fun_def: &mut FunctionDefinition,
                        function_type: FunctionType) -> Result<(), ParseErrorCause> {
        // Track that we're in a function.
        let enclosing_func_type = self.function_type;
        self.function_type = function_type;

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

    fn resolve_local_variable(&mut self, identifier: &str) -> VarLoc {
        let len = self.scopes.len();
        let mut i = len;
        while i > 0 {
            i -= 1;

            let scope = &self.scopes[i];
            match scope.get(identifier) {
                None => (),
                Some(resolve_state) => {
                    let distance = (len - 1 - i) as u16;

                    return VarLoc::new(distance,
                                       resolve_state.frame_index as u16);
                }
            }
        }

        // Couldn't resolve.  This should turn into a runtime error.
        VarLoc::unresolved()
    }

    fn begin_scope(&mut self) {
        if self.scopes.len() >= VAR_LOC_MAX_DISTANCE_USIZE {
            panic!("Too many nested lexical scopes: {}", self.scopes.len());
        }
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, identifier: &str) {
        let scope = self.scopes.last_mut().expect("Resolver::declare: I'm trying to look up the most-local scope, but there are none");
        let var_resolve_state = VarResolveState {
            frame_index: scope.len(),
            is_defined: false,
        };
        ensure_scope_index_limit(scope.len());
        match scope.entry(identifier.to_string()) {
            entry @ Entry::Vacant(_) => entry.or_insert(var_resolve_state),
            Entry::Occupied(_) => panic!("Resolver::declare: I'm trying to declare something that's already declared: {}", identifier),
        };
    }

    fn define(&mut self, identifier: &str) {
        let scope = self.scopes.last_mut().expect("Resolver::define: I'm trying to look up the most-local scope, but there are none");
        let var_resolve_state = VarResolveState {
            frame_index: scope.len(),
            is_defined: true,
        };
        ensure_scope_index_limit(scope.len());
        scope.entry(identifier.to_string())
            .and_modify(|mut state| state.is_defined = true )
            .or_insert(var_resolve_state);
    }
}

fn ensure_scope_index_limit(scope_len: usize) {
    if scope_len >= VAR_LOC_MAX_INDEX_USIZE {
        panic!("Too many unique identifiers in single lexical scope: {}", scope_len);
    }
}
