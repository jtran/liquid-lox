// Location in a source file.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SourceLoc {
    pub line: usize,
}

impl SourceLoc {
    pub fn new(line: usize) -> SourceLoc {
        SourceLoc {
            line,
        }
    }
}

impl Default for SourceLoc {
    fn default() -> SourceLoc {
        SourceLoc {
            line: 1,
        }
    }
}
