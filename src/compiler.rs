use std::borrow::Cow;
use std::convert::TryFrom;

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
}

#[derive(Debug)]
struct Local {
    name: String,
    depth: usize,
}

#[derive(Debug)]
struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
    previous: usize,
    had_error: bool,
    panic_mode: bool,
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
        Compiler {}
    }

    pub fn compile(&mut self, source: &str) -> Result<Chunk, ParseError> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let mut chunk = Chunk::default();

        self.expression(&mut parser, &mut chunk);
        parser.consume(TokenType::Eof, "Expected end of expression");

        // End compiler.
        self.emit_return(&parser, &mut chunk);
        #[cfg(feature = "debug-print-code")]
        if !parser.had_error {
            chunk.disassemble("code");
        }

        if parser.had_error {
            return Err(ParseError::new(vec![ParseErrorCause::new(SourceLoc::default(), "There were errors already output.")]));
        }

        Ok(chunk)
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

    fn emit_op_with_byte_param(&mut self, parser: &Parser, op: Op, byte: u8, chunk: &mut Chunk) {
        let token = parser.previous_token();
        chunk.add_code_op(op, token.line);
        chunk.add_code(byte, token.line);
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

    fn parse_precedence(&mut self, parser: &mut Parser, precedence: Precedence, chunk: &mut Chunk) {
        parser.advance();

        match parser.previous_token().token_type {
            TokenType::LeftParen => self.grouping(parser, chunk),
            TokenType::Minus => self.unary(parser, chunk),
            TokenType::Number => self.number(parser, chunk),
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
                TokenType::Star |
                TokenType::Slash => {
                    self.binary(parser, chunk);
                }
                _ => unreachable!(),
            }
        }
    }

    fn expression(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        self.parse_precedence(parser, Precedence::Assignment, chunk);
    }

    fn binary(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        let op_type = parser.previous_token().token_type;
        let precedence = Precedence::from_operator(op_type);
        let higher_prec = precedence.next().unwrap_or(Precedence::None);
        self.parse_precedence(parser, higher_prec, chunk);

        match op_type {
            TokenType::Plus => self.emit_op(parser, Op::Add, chunk),
            TokenType::Minus => self.emit_op(parser, Op::Subtract, chunk),
            TokenType::Star => self.emit_op(parser, Op::Multiply, chunk),
            TokenType::Slash => self.emit_op(parser, Op::Divide, chunk),
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

    fn unary(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        let op_type = parser.previous_token().token_type;

        // Compile the operand.
        self.parse_precedence(parser, Precedence::Unary, chunk);

        // TODO: Use the line of the token when emitting.  The reference
        // implementation does it this way for simplicity.
        match op_type {
            TokenType::Minus => {
                self.emit_op(parser, Op::Negate, chunk);
            }
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

        let token = &self.tokens[token_index];
        let col_str = match token.token_type {
            TokenType::Eof => Cow::Borrowed("end"),
            _ => Cow::Owned(token.column.to_string()),
        };
        eprintln!("[line {}] Error at {}: {}", token.line, col_str.as_ref(), message);
        self.had_error = true;
    }
}
