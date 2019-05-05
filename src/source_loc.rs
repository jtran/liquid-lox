use crate::token::Token;

// Location in a source file.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SourceLoc {
    pub line: u32,
    pub column: u16,
}

impl SourceLoc {
    pub fn new(line: u32, column: u16) -> SourceLoc {
        SourceLoc {
            line,
            column,
        }
    }
}

impl Default for SourceLoc {
    fn default() -> SourceLoc {
        SourceLoc {
            line: 1,
            column: 1,
        }
    }
}

impl<'a> From<&Token<'a>> for SourceLoc {
    fn from(token: &Token<'a>) -> SourceLoc {
        SourceLoc::new(token.line, token.column)
    }
}
