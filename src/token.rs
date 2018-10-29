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
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'a> {
    token_type: TokenType,
    lexeme: &'a str,
    string_literal: Option<&'a str>,
    float_literal: Option<f64>,
    line: usize,
}

impl <'a> Token<'a> {
    pub fn new(token_type: TokenType,
              lexeme: &'a str,
              string_literal: Option<&'a str>,
              float_literal: Option<f64>,
              line: usize)
        -> Token<'a>
    {
        Token {
            token_type,
            lexeme,
            string_literal,
            float_literal,
            line,
        }
    }
}
