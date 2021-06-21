use std::convert::TryFrom;
use std::rc::Rc;

use num_traits::{FromPrimitive, ToPrimitive};
use num_derive::{FromPrimitive, ToPrimitive};

use crate::error::*;
use crate::scanner::Scanner;
use crate::op::{Chunk, Op};
use crate::source_loc::SourceLoc;
use crate::token::{Token, TokenType};
use crate::value::Value;

pub const U8_COUNT: usize = std::u8::MAX as usize + 1;

#[allow(dead_code)]
pub fn compile(source: &str) -> Result<Chunk, ParseError> {
    let mut compiler = Compiler::new();

    compiler.compile(source)
}

// Precedence levels from lowest to highest.  The important derive is Ord.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, FromPrimitive, ToPrimitive)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

#[derive(Debug)]
pub struct Compiler {
    locals: Vec<Local>,
    // Zero is the global scope.  Each number higher is a level nested deeper.
    scope_depth: u8,
}

#[derive(Debug)]
struct Local {
    name: String,
    // The scope depth that this local variable was declared in.
    depth: LocalDepth,
}

#[derive(Debug, Clone, Copy)]
enum LocalDepth {
    // Declared but not yet defined.
    Uninitialized,
    // The scope depth that this local variable was declared in.
    Depth(u8),
}

#[derive(Debug)]
struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
    previous: usize,
    had_error: bool,
    panic_mode: bool,
    errors: Vec<ParseErrorCause>,
}

impl Precedence {
    pub fn from_operator(token_type: TokenType) -> Precedence {
        match token_type {
            TokenType::LeftParen => Precedence::Call,
            TokenType::RightParen |
            TokenType::LeftBrace |
            TokenType::RightBrace |
            TokenType::LeftBracket |
            TokenType::RightBracket |
            TokenType::Comma => Precedence::None,
            TokenType::Dot => Precedence::Call,
            TokenType::Minus => Precedence::Term,
            TokenType::Plus => Precedence::Term,
            TokenType::Semicolon => Precedence::None,
            TokenType::Slash => Precedence::Factor,
            TokenType::Star => Precedence::Factor,
            TokenType::Bang => Precedence::None,
            TokenType::BangEqual => Precedence::Equality,
            TokenType::Equal => Precedence::None,
            TokenType::EqualEqual => Precedence::Equality,
            TokenType::Greater |
            TokenType::GreaterEqual |
            TokenType::Less |
            TokenType::LessEqual => Precedence::Comparison,
            TokenType::Identifier |
            TokenType::String |
            TokenType::Number => Precedence::None,
            TokenType::And => Precedence::And,
            TokenType::Break |
            TokenType::Class |
            TokenType::Continue |
            TokenType::Else |
            TokenType::False |
            TokenType::Fun |
            TokenType::For |
            TokenType::If |
            TokenType::Nil => Precedence::None,
            TokenType::Or => Precedence::Or,
            TokenType::Print |
            TokenType::Return |
            TokenType::Super |
            TokenType::This |
            TokenType::True |
            TokenType::Var |
            TokenType::While |
            TokenType::Eof => Precedence::None,
        }
    }

    pub fn next(&self) -> Option<Precedence> {
        match self.to_u8() {
            None => None,
            Some(byte) => Precedence::from_u8(byte.saturating_add(1)),
        }
    }
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            locals: Vec::with_capacity(U8_COUNT),
            scope_depth: 0,
        }
    }

    pub fn compile(&mut self, source: &str) -> Result<Chunk, ParseError> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let mut chunk = Chunk::default();

        while !parser.matches(TokenType::Eof) {
            self.declaration(&mut parser, &mut chunk);
        }

        // End compiler.
        self.emit_return(&parser, &mut chunk);
        #[cfg(feature = "debug-print-code")]
        if !parser.had_error {
            chunk.disassemble("code");
        }

        if parser.had_error {
            return Err(ParseError::new(parser.take_errors()));
        }

        Ok(chunk)
    }

    fn begin_scope(&mut self) {
        assert!(self.scope_depth < u8::MAX);
        self.scope_depth += 1;
    }

    fn end_scope(&mut self, parser: &Parser, chunk: &mut Chunk) {
        assert!(self.scope_depth > 0);
        self.scope_depth -= 1;

        // Pop the local variables of the scope that's ending.
        loop {
            match self.locals.last() {
                None => break,
                Some(local) => {
                    match local.depth {
                        LocalDepth::Uninitialized => break,
                        LocalDepth::Depth(depth) => {
                            if depth <= self.scope_depth {
                                break;
                            }
                        }
                    }
                }
            }
            self.emit_op(parser, Op::Pop, chunk);
            self.locals.pop();
        }
    }

    fn emit_constant(&mut self, parser: &mut Parser, value: Value, chunk: &mut Chunk) {
        let byte = self.make_constant(parser, value, chunk);
        self.emit_op_with_byte_param(parser, Op::Constant, byte, chunk)
    }

    fn emit_return(&mut self, parser: &Parser, chunk: &mut Chunk) {
        self.emit_op(parser, Op::Return, chunk)
    }

    fn emit_op(&mut self, parser: &Parser, op: Op, chunk: &mut Chunk) {
        let token = parser.previous_token();
        chunk.add_code_op(op, token.line);
    }

    fn emit_ops(&mut self, parser: &Parser, op1: Op, op2: Op, chunk: &mut Chunk) {
        let token = parser.previous_token();
        chunk.add_code_op(op1, token.line);
        chunk.add_code_op(op2, token.line);
    }

    fn emit_op_with_byte_param(&mut self, parser: &Parser, op: Op, byte: u8, chunk: &mut Chunk) {
        let token = parser.previous_token();
        chunk.add_code_op(op, token.line);
        chunk.add_code(byte, token.line);
    }

    fn emit_loop(&mut self, parser: &mut Parser, loop_start: usize, chunk: &mut Chunk) {
        let line = parser.previous_token().line;
        chunk.add_code_op(Op::Loop, line);
        // The extra 2 is to adjust for the delta itself in the bytecode.
        let cur_len = chunk.code_len();
        assert!(cur_len >= loop_start - 2);
        let delta = cur_len - loop_start + 2;

        if delta > usize::from(u16::MAX) {
            parser.error_from_last("Loop body too large.");
        }

        chunk.add_code(((delta >> 8) & 0xff) as u8, line);
        chunk.add_code((delta & 0xff) as u8, line);
    }

    fn emit_jump(&mut self, parser: &Parser, op: Op, chunk: &mut Chunk) -> usize {
        let token = parser.previous_token();
        chunk.add_code_op(op, token.line);
        // Location of jump amount that will be patched later.
        let jump_location_index = chunk.code_len();
        // Placeholder jump amount.  If you change this, you also need to change
        // patch_jump().
        chunk.add_code(u8::MAX, token.line);
        chunk.add_code(u8::MAX, token.line);

        jump_location_index
    }

    fn make_constant(&mut self, parser: &mut Parser, value: Value, chunk: &mut Chunk) -> u8 {
        let constant = chunk.add_constant(value);
        match u8::try_from(constant) {
            Err(_) => {
                parser.error_from_last("Too many constants in one chunk.");
                return 0;
            }
            Ok(byte) => byte,
        }
    }

    fn patch_jump(&mut self, parser: &mut Parser, jump_index: usize, chunk: &mut Chunk) {
        // The extra 2 is to adjust for the delta itself in the bytecode.  If
        // you change this, you also need to change emit_jump().
        let cur_len = chunk.code_len();
        assert!(cur_len >= jump_index + 2);
        let jump_delta = cur_len - jump_index - 2;

        if jump_delta > usize::from(u16::MAX) {
            parser.error_from_last("Too much code to jump over.");
        }

        chunk.set_code_byte(jump_index, ((jump_delta >> 8) & 0xff) as u8);
        chunk.set_code_byte(jump_index + 1, (jump_delta & 0xff) as u8);
    }

    fn parse_precedence(&mut self, parser: &mut Parser, precedence: Precedence, chunk: &mut Chunk) {
        parser.advance();

        let can_assign = precedence <= Precedence::Assignment;

        match parser.previous_token().token_type {
            TokenType::LeftParen => self.grouping(parser, chunk),
            TokenType::Minus => self.unary(parser, chunk),
            TokenType::Bang => self.unary(parser, chunk),
            TokenType::Identifier => self.variable(parser, can_assign, chunk),
            TokenType::String => self.string(parser, chunk),
            TokenType::Number => self.number(parser, chunk),
            TokenType::False => self.literal(parser, chunk),
            TokenType::Nil => self.literal(parser, chunk),
            TokenType::True => self.literal(parser, chunk),
            _ => {
                parser.error_from_last("Expect expression.");
                return;
            }
        }

        loop {
            let cur_token_type = parser.current_token().token_type;
            let cur_token_prec = Precedence::from_operator(cur_token_type);
            if precedence > cur_token_prec {
                break;
            }

            parser.advance();
            match parser.previous_token().token_type {
                TokenType::Minus |
                TokenType::Plus |
                TokenType::Slash |
                TokenType::Star |
                TokenType::BangEqual |
                TokenType::EqualEqual |
                TokenType::Greater |
                TokenType::GreaterEqual |
                TokenType::Less |
                TokenType::LessEqual => {
                    self.binary(parser, chunk);
                }
                _ => unreachable!(),
            }

            if can_assign && parser.matches(TokenType::Equal) {
                parser.error_from_last("Invalid assignment target.");
                return;
            }
        }
    }

    fn identifier_constant(&mut self, parser: &mut Parser, identifier: String, chunk: &mut Chunk) -> u8 {
        self.make_constant(parser, Value::StringVal(Rc::new(identifier)), chunk)
    }

    fn resolve_local(&self, parser: &mut Parser, name: &str) -> Option<u8> {
        // Look at most recent locals first so that new variables shadow old
        // ones.
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                match local.depth {
                    LocalDepth::Uninitialized => {
                        parser.error_from_last("Cannot read local variable in its own initializer.");
                    }
                    LocalDepth::Depth(_) => {}
                }
                assert!(i < u8::MAX as usize);
                return Some(i as u8);
            }
        }
        None
    }

    fn add_local(&mut self, parser: &mut Parser, name: String) {
        if self.locals.len() == U8_COUNT {
            parser.error_from_last("Too many local variables in function.");
            return;
        }

        self.locals.push(Local {
            name,
            depth: LocalDepth::Uninitialized,
        });
    }

    fn declare_variable(&mut self, parser: &mut Parser) {
        // Nothing to do for global variables.
        if self.scope_depth == 0 { return; }

        let name = parser.previous_token().lexeme.to_string();
        for local in self.locals.iter().rev() {
            // If we reach an enclosing scope, we're done.
            match local.depth {
                LocalDepth::Uninitialized => {}
                LocalDepth::Depth(depth) => {
                    if depth < self.scope_depth {
                        break;
                    }
                }
            }

            if name == local.name {
                parser.error_from_last("Already variable with this name in this scope.");
                // Should we break here?  The book doesn't.
            }
        }

        self.add_local(parser, name);
    }

    // Returns the constant index.
    fn parse_variable(&mut self, parser: &mut Parser, error_message: &str, chunk: &mut Chunk) -> Option<u8> {
        parser.consume(TokenType::Identifier, error_message);

        self.declare_variable(parser);
        // Local variables aren't looked up by name, so we don't need to create
        // a constant.
        if self.scope_depth > 0 { return None; }

        let identifier = parser.previous_token().lexeme.to_string();

        Some(self.identifier_constant(parser, identifier, chunk))
    }

    fn mark_initialized(&mut self) {
        match self.locals.last_mut() {
            None => {
                panic!("Tried to mark initialized when there are no locals");
            }
            Some(local) => {
                local.depth = LocalDepth::Depth(self.scope_depth);
            }
        }
    }

    fn define_variable(&mut self, parser: &mut Parser, global: Option<u8>, chunk: &mut Chunk) {
        if let Some(global) = global {
            assert!(self.scope_depth == 0);
            self.emit_op_with_byte_param(parser, Op::DefineGlobal, global, chunk)
        } else {
            // Nothing to do at runtime for a local variable.  The temporary
            // initial value on the stack becomes the local variable slot.
            self.mark_initialized();
        }
    }

    fn expression(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        self.parse_precedence(parser, Precedence::Assignment, chunk);
    }

    fn block(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        while !parser.check(TokenType::RightBrace) && !parser.check(TokenType::Eof) {
            self.declaration(parser, chunk);
        }

        parser.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn var_declaration(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        let global = self.parse_variable(parser, "Expect variable name.", chunk);

        if parser.matches(TokenType::Equal) {
            self.expression(parser, chunk);
        } else {
            self.emit_op(parser, Op::Nil, chunk);
        }
        parser.consume(TokenType::Semicolon, "Expect ';' after variable declaration.");

        self.define_variable(parser, global, chunk);
    }

    fn expression_statement(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        self.expression(parser, chunk);
        parser.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_op(parser, Op::Pop, chunk);
    }

    fn for_statement(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        // Any variable declared in the initializer should be scoped to the
        // for-loop.
        self.begin_scope();
        parser.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if parser.matches(TokenType::Semicolon) {
            // No initialize statement.
        } else if parser.matches(TokenType::Var) {
            self.var_declaration(parser, chunk);
        } else {
            self.expression_statement(parser, chunk);
        }

        let mut loop_start = chunk.code_len();
        let mut exit_jump = None;
        // Loop condition.
        if !parser.matches(TokenType::Semicolon) {
            self.expression(parser, chunk);
            parser.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            exit_jump = Some(self.emit_jump(parser, Op::JumpIfFalse, chunk));
            // Pop the condition.
            self.emit_op(parser, Op::Pop, chunk);
        }

        // Loop increment.
        if !parser.matches(TokenType::RightParen) {
            let body_jump = self.emit_jump(parser, Op::Jump, chunk);
            let increment_start = chunk.code_len();
            self.expression(parser, chunk);
            self.emit_op(parser, Op::Pop, chunk);
            parser.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(parser, loop_start, chunk);
            loop_start = increment_start;
            self.patch_jump(parser, body_jump, chunk);
        }

        // Loop body.
        self.statement(parser, chunk);
        self.emit_loop(parser, loop_start, chunk);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(parser, exit_jump, chunk);
            // Pop the condition.
            self.emit_op(parser, Op::Pop, chunk);
        }

        self.end_scope(parser, chunk);
    }

    fn if_statement(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        parser.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression(parser, chunk);
        parser.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(parser, Op::JumpIfFalse, chunk);
        // Pop the condition in the truthy case.
        self.emit_op(parser, Op::Pop, chunk);
        self.statement(parser, chunk);

        let else_jump = self.emit_jump(parser, Op::Jump, chunk);

        self.patch_jump(parser, then_jump, chunk);
        // Pop the condition in the falsey case.
        self.emit_op(parser, Op::Pop, chunk);

        if parser.matches(TokenType::Else) {
            self.statement(parser, chunk);
        }
        self.patch_jump(parser, else_jump, chunk);
    }

    fn while_statement(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        let loop_start = chunk.code_len();
        parser.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression(parser, chunk);
        parser.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(parser, Op::JumpIfFalse, chunk);
        // Pop the condition in the truthy case.
        self.emit_op(parser, Op::Pop, chunk);
        self.statement(parser, chunk);
        self.emit_loop(parser, loop_start, chunk);

        self.patch_jump(parser, exit_jump, chunk);
        // Pop the condition in the falsey case.
        self.emit_op(parser, Op::Pop, chunk);
    }

    fn print_statement(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        self.expression(parser, chunk);
        parser.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_op(parser, Op::Print, chunk);
    }

    fn return_statement(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        if !parser.matches(TokenType::Semicolon) {
            self.expression(parser, chunk);
            parser.consume(TokenType::Semicolon, "Expect ';' after return value.");
        }
        self.emit_op(parser, Op::Return, chunk);
    }

    fn declaration(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        if parser.matches(TokenType::Var) {
            self.var_declaration(parser, chunk);
        } else {
            self.statement(parser, chunk);
        }

        if parser.panic_mode {
            parser.synchronize();
        }
    }

    fn statement(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        if parser.matches(TokenType::Print) {
            self.print_statement(parser, chunk);
        } else if parser.matches(TokenType::For) {
            self.for_statement(parser, chunk);
        } else if parser.matches(TokenType::If) {
            self.if_statement(parser, chunk);
        } else if parser.matches(TokenType::While) {
            self.while_statement(parser, chunk);
        } else if parser.matches(TokenType::LeftBrace) {
            self.begin_scope();
            self.block(parser, chunk);
            self.end_scope(parser, chunk);
        } else if parser.matches(TokenType::Return) {
            self.return_statement(parser, chunk);
        } else {
            self.expression_statement(parser, chunk);
        }
    }

    fn binary(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        let op_type = parser.previous_token().token_type;
        let precedence = Precedence::from_operator(op_type);
        let higher_prec = precedence.next().unwrap_or(Precedence::None);
        self.parse_precedence(parser, higher_prec, chunk);

        match op_type {
            TokenType::Plus => self.emit_op(parser, Op::Add, chunk),
            TokenType::Minus => self.emit_op(parser, Op::Subtract, chunk),
            TokenType::Slash => self.emit_op(parser, Op::Divide, chunk),
            TokenType::Star => self.emit_op(parser, Op::Multiply, chunk),
            TokenType::BangEqual => {
                self.emit_ops(parser, Op::Equal, Op::Not, chunk);
            }
            TokenType::EqualEqual => self.emit_op(parser, Op::Equal, chunk),
            TokenType::Greater => self.emit_op(parser, Op::Greater, chunk),
            TokenType::GreaterEqual => {
                self.emit_ops(parser, Op::Less, Op::Not, chunk);
            }
            TokenType::Less => self.emit_op(parser, Op::Less, chunk),
            TokenType::LessEqual => {
                self.emit_ops(parser, Op::Greater, Op::Not, chunk);
            }
            _ => unreachable!(),
        }
    }

    fn literal(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        match parser.previous_token().token_type {
            TokenType::False => self.emit_op(parser, Op::False, chunk),
            TokenType::Nil => self.emit_op(parser, Op::Nil, chunk),
            TokenType::True => self.emit_op(parser, Op::True, chunk),
            _ => unreachable!(),
        }
    }

    fn number(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        match parser.previous_token().float_literal {
            None => {
                panic!("Expected float literal on token; found {:?}", parser.previous_token());
            }
            Some(value) => {
                self.emit_constant(parser, Value::NumberVal(value), chunk);
            }
        }
    }

    fn string(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        match parser.previous_token().string_literal {
            None => {
                panic!("Expected string literal on token; found {:?}", parser.previous_token());
            }
            Some(string) => {
                let value = Value::StringVal(Rc::new(string.to_owned()));
                self.emit_constant(parser, value, chunk);
            }
        }
    }

    fn named_variable(&mut self, parser: &mut Parser, identifier: String, can_assign: bool, chunk: &mut Chunk) {
        let local_index = self.resolve_local(parser, &identifier);
        let (get_op, set_op, arg) = if let Some(local_index) = local_index {
            (Op::GetLocal, Op::SetLocal, local_index)
        } else {
            let constant_index = self.identifier_constant(parser, identifier, chunk);
            (Op::GetGlobal, Op::SetGlobal, constant_index)
        };

        if can_assign && parser.matches(TokenType::Equal) {
            self.expression(parser, chunk);
            self.emit_op_with_byte_param(parser, set_op, arg, chunk);
        } else {
            self.emit_op_with_byte_param(parser, get_op, arg, chunk);
        }
    }

    fn variable(&mut self, parser: &mut Parser, can_assign: bool, chunk: &mut Chunk) {
        self.named_variable(parser, parser.previous_token().lexeme.to_string(), can_assign, chunk);
    }

    fn unary(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        let op_type = parser.previous_token().token_type;

        // Compile the operand.
        self.parse_precedence(parser, Precedence::Unary, chunk);

        // TODO: Use the line of the token when emitting.  The reference
        // implementation does it this way for simplicity.
        match op_type {
            TokenType::Minus => self.emit_op(parser, Op::Negate, chunk),
            TokenType::Bang => self.emit_op(parser, Op::Not, chunk),
            _ => unreachable!(),
        }
    }

    fn grouping(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        self.expression(parser, chunk);
        parser.consume(TokenType::RightParen, "Expect ')' after expression.");
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Parser {
        Parser {
            tokens,
            current: 0,
            previous: 0,
            had_error: false,
            panic_mode: false,
            errors: Vec::new(),
        }
    }

    pub fn current_token(&self) -> &Token {
        &self.tokens[self.current]
    }

    pub fn previous_token(&self) -> &Token {
        &self.tokens[self.previous]
    }

    pub fn advance(&mut self) {
        self.previous = self.current;
        self.current = self.current.saturating_add(1);
    }

    pub fn consume(&mut self, token_type: TokenType, message: &str) {
        if self.tokens[self.current].token_type == token_type {
            self.advance();
        } else {
            self.error_at_current(message);
        }
    }

    pub fn check(&self, token_type: TokenType) -> bool {
        self.tokens[self.current].token_type == token_type
    }

    pub fn matches(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) { return false; }
        self.advance();

        true
    }

    pub fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current_token().token_type != TokenType::Eof {
            if self.previous_token().token_type == TokenType::Semicolon {
                return;
            }

            match self.current_token().token_type {
                TokenType::Class |
                TokenType::Fun |
                TokenType::Var |
                TokenType::For |
                TokenType::If |
                TokenType::While |
                TokenType::Print |
                TokenType::Return => { return; }
                _ => {}
            }

            self.advance();
        }
    }

    pub fn take_errors(&mut self) -> Vec<ParseErrorCause> {
        self.errors.drain(..).collect()
    }

    fn error_from_last(&mut self, message: &str) {
        self.error_at(self.previous, message);
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current, message);
    }

    fn error_at(&mut self, token_index: usize, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        self.had_error = true;

        let token = &self.tokens[token_index];
        self.errors.push(ParseErrorCause::new_with_location(SourceLoc::new(token.line, token.column),
                                                            token.lexeme,
                                                            message))
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_precedence_order() {
        assert!(Precedence::None < Precedence::Assignment);
        assert!(Precedence::Assignment < Precedence::Or);
    }
}
