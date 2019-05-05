#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals.
    Identifier, String, Number,

    // Keywords.
    And, Break, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub string_literal: Option<&'a str>,
    pub float_literal: Option<f64>,
    pub line: u32,
    pub column: u16,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType,
              lexeme: &'a str,
              string_literal: Option<&'a str>,
              float_literal: Option<f64>,
              line: u32,
              column: u16)
        -> Token<'a>
    {
        Token {
            token_type,
            lexeme,
            string_literal,
            float_literal,
            line,
            column,
        }
    }
}
