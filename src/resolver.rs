use std::cell::Cell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::ops::Deref;

use crate::ast::*;
use crate::environment::*;
use crate::error::*;
use crate::source_loc::*;
use crate::value::*;

pub fn resolve(statements: &mut Vec<Stmt>) -> Result<(), ParseError> {
    let mut resolver = Resolver::new();
    resolver.resolve(statements)?;

    Ok(())
}

#[allow(dead_code)]
pub fn resolve_expression(expression: &mut Expr) -> Result<(), ParseError> {
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
    class_type: ClassType,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum VarResolveDefinedState {
    UndefinedVar,
    DeclaredVar,
    DefinedVar,
}
use VarResolveDefinedState::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VarResolveState {
    pub frame_index: usize,
    pub defined_state: VarResolveDefinedState,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum FunctionType {
    NoFunction,
    Plain,
    Initializer,
    Method,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ClassType {
    NoClass,
    PlainClass,
    Subclass,
}

impl Resolver {
    pub fn new() -> Resolver {
        let mut resolver = Resolver {
            scopes: Vec::with_capacity(1),
            function_type: FunctionType::NoFunction,
            class_type: ClassType::NoClass,
        };

        // Start with the globals scope.
        //
        // Note: this behavior must match the Interpreter.
        resolver.begin_scope();
        for native_id in all_native_ids() {
            let result = resolver.define(&native_id.to_string(), &SourceLoc::default());
            if let Err(error) = result {
                panic!("Resolver::new(): defining native function failed: {}", error.message);
            }
        }

        resolver
    }

    pub fn resolve(&mut self, statements: &mut Vec<Stmt>) -> Result<(), ParseError> {
        let mut errors = Vec::new();
        for statement in statements.iter_mut() {
            let result = self.resolve_statement(statement);
            if let Err(error) = result {
                errors.push(error);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(ParseError::new(errors))
        }
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
                // Track that we're in a class.
                let enclosing_class_type = self.class_type;
                self.class_type = if class_def.has_superclass() {
                    ClassType::Subclass
                } else {
                    ClassType::PlainClass
                };

                let result = self.resolve_class(class_def);

                // Restore previous class type.
                self.class_type = enclosing_class_type;

                result
            }
            Stmt::Continue(_) => Ok(()),
            Stmt::Expression(expr) => self.resolve_expression(expr),
            Stmt::Fun(fun_def) => {
                self.define(&fun_def.name, &fun_def.source_loc)?;

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
                    FunctionType::NoFunction => return Err(ParseErrorCause::new_with_location(*loc, "return", "Cannot return from top-level code.")),
                }

                // TODO: This currently doesn't distinguish between returning
                // nil and no return value.
                if self.function_type == FunctionType::Initializer {
                    match expr {
                        Expr::LiteralNil => (),
                        _ => {
                            return Err(ParseErrorCause::new_with_location(*loc, "return", "Cannot return a value from an initializer."));
                        }
                    }
                }

                self.resolve_expression(expr)
            }
            Stmt::Var(identifier, frame_index_cell, expr, loc) => {
                self.declare(identifier, frame_index_cell, loc)?;
                self.resolve_expression(expr)?;
                self.define(identifier, loc)?;

                Ok(())
            }
            Stmt::While(condition, body) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;

                Ok(())
            }
            Stmt::WhileIncrement(condition, body, increment) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;
                self.resolve_expression(increment)?;

                Ok(())
            }
        }
    }

    pub fn resolve_expression(&mut self, expression: &mut Expr) -> Result<(), ParseErrorCause> {
        match expression {
            Expr::Assign(identifier, dist_cell, expr, loc) => {
                self.resolve_expression(expr)?;
                let var_loc = self.resolve_local_variable(identifier, loc)?;
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
            Expr::GetIndex(expr, index, _) => {
                self.resolve_expression(expr)?;
                self.resolve_expression(index)?;

                Ok(())
            }
            Expr::Grouping(expr) => self.resolve_expression(expr),
            Expr::LiteralArray(elements) => {
                for element in elements.iter_mut() {
                    self.resolve_expression(element)?;
                }

                Ok(())
            }
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
                // Note: This is in reverse order from the current reference
                // implementation so that it matches the interpreter evaluation
                // order.
                self.resolve_expression(object_expr)?;
                self.resolve_expression(value_expr)?;

                Ok(())
            }
            Expr::SetIndex(array, index, rhs, _) => {
                self.resolve_expression(array)?;
                self.resolve_expression(index)?;
                self.resolve_expression(rhs)?;

                Ok(())
            }
            Expr::Super(super_dist_cell, _, loc) => {
                match self.class_type {
                    ClassType::PlainClass | ClassType::Subclass => (),
                    ClassType::NoClass => {
                        return Err(ParseErrorCause::new_with_location(*loc, "super", "Cannot use 'super' outside of a class."));
                    }
                }

                match self.class_type {
                    ClassType::Subclass => (),
                    _ => {
                        return Err(ParseErrorCause::new_with_location(*loc, "super", "Cannot use 'super' in a class with no superclass."));
                    }
                }

                let super_var_loc = self.resolve_local_variable("super", loc)?;
                super_dist_cell.set(super_var_loc);

                Ok(())
            }
            Expr::Unary(_, right, _) => {
                self.resolve_expression(right)?;

                Ok(())
            }
            Expr::Variable(identifier, dist_cell, loc) => {
                match self.class_type {
                    ClassType::PlainClass | ClassType::Subclass => (),
                    ClassType::NoClass => {
                        if identifier == "this" {
                            return Err(ParseErrorCause::new_with_location(*loc, "this", "Cannot use 'this' outside of a class."));
                        }
                    }
                }

                // Scope to drop the borrow of self.
                {
                    let scope = self.scopes.last().expect("Resolver::resolve_expression: last scope to be present");
                    match scope.get(identifier) {
                        None => (),
                        Some(ref resolve_state) => {
                            match resolve_state.defined_state {
                                DefinedVar | UndefinedVar => (),
                                DeclaredVar => {
                                    return Err(ParseErrorCause::new_with_location(*loc, identifier, "Cannot read local variable in its own initializer."));
                                }
                            }
                        }
                    };
                }

                let var_loc = self.resolve_local_variable(identifier, loc)?;
                dist_cell.set(var_loc);

                Ok(())
            }
        }
    }

    fn resolve_class(&mut self,
                     class_def: &mut ClassDefinition) -> Result<(), ParseErrorCause> {
        self.define(&class_def.name, &class_def.source_loc)?;

        // Superclass.
        match &class_def.superclass {
            None => (),
            Some(boxed_expr) => {
                match boxed_expr.deref() {
                    Expr::Variable(id, _, loc) => {
                        if *id == class_def.name {
                            return Err(ParseErrorCause::new_with_location(*loc, id, "A class cannot inherit from itself."));
                        }
                    }
                    _ => (),
                }
            }
        }
        match &mut class_def.superclass {
            None => (),
            Some(super_expr) => {
                self.resolve_expression(super_expr)?;

                self.begin_scope();
                let result = self.define("super", &class_def.source_loc);
                if result.is_err() {
                    // Ensure we preserve state.
                    self.end_scope();
                    return result;
                }
            }
        }

        self.begin_scope();
        let mut result = self.define("this", &class_def.source_loc);

        if result.is_ok() {
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
        }

        // "this" scope.
        self.end_scope();

        // "super" scope.
        if class_def.superclass.is_some() {
            self.end_scope();
        }

        result
    }

    fn resolve_function(&mut self,
                        fun_def: &mut FunctionDefinition,
                        function_type: FunctionType) -> Result<(), ParseErrorCause> {
        // Track what kind of function we're in.
        let enclosing_func_type = self.function_type;
        // We can't just use the new one because it may be a plain function
        // nested inside of a method.  In that case, we want to keep the state
        // that we are in a method.
        self.function_type = function_type;

        self.begin_scope();
        let mut result = Ok(());
        for parameter in &fun_def.parameters {
            result = self.define(&parameter.name, &parameter.source_loc);
            if result.is_err() {
                break;
            }
        }
        if result.is_ok() {
            for mut body_statement in &mut fun_def.body {
                result = self.resolve_statement(&mut body_statement);
                if result.is_err() {
                    break;
                }
            }
        }
        self.end_scope();

        // Restore previous function type.
        self.function_type = enclosing_func_type;

        result
    }

    fn resolve_local_variable(&mut self,
                              identifier: &str,
                              loc: &SourceLoc) -> Result<VarLoc, ParseErrorCause> {
        let len = self.scopes.len();
        let mut i = len;
        while i > 0 {
            i -= 1;

            let scope = &self.scopes[i];
            match scope.get(identifier) {
                None => (),
                Some(resolve_state) => {
                    let distance = (len - 1 - i) as u16;

                    return Ok(VarLoc::new(distance,
                                          resolve_state.frame_index as u8));
                }
            }
        }

        // Couldn't resolve.  This should turn into a global variable access or
        // runtime error.
        let frame_index = self.forward_reserve_global_var(identifier, loc)?;
        // There should always be a global scope.
        assert!(len > 0);
        let distance = (len - 1) as u16;

        Ok(VarLoc::new_global(distance, frame_index as u8))
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

    fn is_in_global_scope(&self) -> bool {
        self.scopes.len() == 1
    }

    fn forward_reserve_global_var(&mut self,
                                  identifier: &str,
                                  loc: &SourceLoc) -> Result<usize, ParseErrorCause> {
        let scope = self.scopes.first_mut().expect("Resolver::forward_reserve_global_var: I'm trying to look up the top-most global scope, but there are none");
        let frame_index = scope.len();
        let var_resolve_state = VarResolveState {
            frame_index,
            defined_state: UndefinedVar,
        };
        ensure_scope_index_limit(scope.len(), identifier, loc)?;
        match scope.entry(identifier.to_string()) {
            entry @ Entry::Vacant(_) => entry.or_insert(var_resolve_state),
            Entry::Occupied(_) => panic!("Resolver::forward_reserve_global_var: I'm trying to forward reserve something that's already declared: {}", identifier),
        };

        Ok(frame_index)
    }

    fn declare(&mut self,
               identifier: &str,
               frame_index_cell: &mut Cell<FrameIndex>,
               loc: &SourceLoc) -> Result<(), ParseErrorCause> {
        let scope = self.scopes.last_mut().expect("Resolver::declare: I'm trying to look up the most-local scope, but there are none");
        let mut frame_index = scope.len();
        let var_resolve_state = VarResolveState {
            frame_index,
            defined_state: DeclaredVar,
        };
        ensure_scope_index_limit(scope.len(), identifier, loc)?;

        let mut already_declared = false;
        scope.entry(identifier.to_string())
            .and_modify(|mut state| {
                match state.defined_state {
                    UndefinedVar => {
                        state.defined_state = DeclaredVar;
                        frame_index = state.frame_index;
                    }
                    DeclaredVar | DefinedVar => {
                        already_declared = true;
                        frame_index = state.frame_index;
                    }
                };
            })
            .or_insert(var_resolve_state);

        if already_declared {
            if self.is_in_global_scope() {
                // Refer to the previously declared global.
                frame_index_cell.set(FrameIndex::new(frame_index as u8))
            } else {
                return Err(ParseErrorCause::new_with_location(*loc, identifier, "Variable with this name already declared in this scope."));
            }
        } else {
            // Refer to the previously declared global.
            frame_index_cell.set(FrameIndex::new(frame_index as u8));
        }

        Ok(())
    }

    fn define(&mut self, identifier: &str, loc: &SourceLoc) -> Result<(), ParseErrorCause> {
        let scope = self.scopes.last_mut().expect("Resolver::define: I'm trying to look up the most-local scope, but there are none");
        let var_resolve_state = VarResolveState {
            frame_index: scope.len(),
            defined_state: DefinedVar,
        };
        ensure_scope_index_limit(scope.len(), identifier, loc)?;
        let mut already_defined = false;
        scope.entry(identifier.to_string())
            .and_modify(|mut state| {
                match state.defined_state {
                    UndefinedVar | DeclaredVar => state.defined_state = DefinedVar,
                    DefinedVar => already_defined = true,
                }
            })
            .or_insert(var_resolve_state);

        if already_defined && !self.is_in_global_scope() {
            return Err(ParseErrorCause::new_with_location(*loc, identifier, "Variable with this name already declared in this scope."));
        }

        Ok(())
    }
}

// The 256th local should fail.
fn ensure_scope_index_limit(new_scope_len: usize,
                            identifier: &str,
                            loc: &SourceLoc) -> Result<(), ParseErrorCause> {
    if new_scope_len > VAR_LOC_MAX_INDEX_USIZE {
        Err(ParseErrorCause::new_with_location(*loc, identifier, "Too many local variables in function."))
    } else {
        Ok(())
    }
}
