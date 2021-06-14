use num_traits::{FromPrimitive, ToPrimitive};
use num_derive::{FromPrimitive, ToPrimitive};
use crate::Value;

#[derive(Debug, Copy, Clone, FromPrimitive, ToPrimitive)]
pub enum Op {
    Constant,

    Add,
    Subtract,
    Multiply,
    Divide,

    Print,

    Return,
}

#[derive(Debug, Default)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new(code: Vec<u8>, constants: Vec<Value>) -> Chunk {
        Chunk {
            code,
            constants,
        }
    }

    pub fn add_code_op(&mut self, op: Op) {
        self.code.push(op.to_u8().unwrap_or_else(||
            panic!("couldn't convert op to byte: {:?}", op)
        ));
    }

    #[allow(dead_code)]
    pub fn add_code(&mut self, byte: u8) {
        self.code.push(byte);
    }

    #[allow(dead_code)]
    pub fn add_constant(&mut self, constant: Value) {
        self.constants.push(constant);
    }

    pub fn get_op_code(&self, ip: usize) -> Option<Op> {
        Op::from_u8(self.code[ip])
    }

    pub fn code_byte(&self, ip: usize) -> u8 {
        self.code[ip]
    }

    pub fn constant(&self, index: usize) -> Value {
        self.constants[index].clone()
    }
}
