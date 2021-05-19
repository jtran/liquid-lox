use num_traits::{FromPrimitive, ToPrimitive};
use num_derive::{FromPrimitive, ToPrimitive};
use crate::Value;

#[derive(Debug, Copy, Clone, FromPrimitive, ToPrimitive)]
pub enum Op {
    Constant,
    Nil,
    True,
    False,

    Equal,
    Greater,
    Less,

    Add,
    Subtract,
    Multiply,
    Divide,

    Not,
    Negate,

    Print,

    Return,
}

#[derive(Debug, Default)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<u32>,
}

impl Chunk {
    #[allow(dead_code)]
    pub fn new(code: Vec<u8>, constants: Vec<Value>, lines: Vec<u32>) -> Chunk {
        Chunk {
            code,
            constants,
            lines,
        }
    }

    pub fn add_code_op(&mut self, op: Op, line: u32) {
        self.code.push(op.to_u8().unwrap_or_else(||
            panic!("couldn't convert op to byte: {:?}", op)
        ));
        self.lines.push(line);
    }

    pub fn add_code(&mut self, byte: u8, line: u32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, constant: Value) -> usize {
        let index = self.constants.len();
        self.constants.push(constant);

        index
    }

    pub fn get_op_code(&self, index: usize) -> Option<Op> {
        Op::from_u8(self.code[index])
    }

    pub fn code_byte(&self, index: usize) -> u8 {
        self.code[index]
    }

    pub fn constant(&self, index: usize) -> Value {
        self.constants[index].clone()
    }

    pub fn line(&self, index: usize) -> u32 {
        self.lines[index]
    }

    #[allow(dead_code)]
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);

        let mut offset = 0;
        let len = self.code.len();
        loop {
            if offset >= len { break; }
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        let instruction = self.code[offset];
        match Op::from_u8(instruction) {
            None => {
                println!("Unknown opcode {}", instruction);
                offset + 1
            }
            Some(op @ Op::Constant) => self.constant_instruction(op, offset),
            Some(op @ Op::Nil) |
            Some(op @ Op::True) |
            Some(op @ Op::False) |
            Some(op @ Op::Equal) |
            Some(op @ Op::Greater) |
            Some(op @ Op::Less) |
            Some(op @ Op::Add) |
            Some(op @ Op::Subtract) |
            Some(op @ Op::Multiply) |
            Some(op @ Op::Divide) |
            Some(op @ Op::Not) |
            Some(op @ Op::Negate) |
            Some(op @ Op::Print) |
            Some(op @ Op::Return) => self.simple_instruction(op, offset),
        }
    }

    fn simple_instruction(&self, op: Op, offset: usize) -> usize {
        println!("{}", op);

        offset + 1
    }

    fn constant_instruction(&self, op: Op, offset: usize) -> usize {
        let constant_index = self.code[offset + 1];
        let value = &self.constants[usize::from(constant_index)];
        println!("{:<16} {:>4} '{}'",
                 op,
                 constant_index,
                 value.to_runtime_string());

        offset + 2
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::Op::*;
        match self {
            Constant => write!(f, "OP_CONSTANT"),
            Nil => write!(f, "OP_NIL"),
            True => write!(f, "OP_TRUE"),
            False => write!(f, "OP_FALSE"),
            Equal => write!(f, "OP_EQUAL"),
            Greater => write!(f, "OP_GREATER"),
            Less => write!(f, "OP_LESS"),
            Add => write!(f, "OP_ADD"),
            Subtract => write!(f, "OP_SUBTRACT"),
            Multiply => write!(f, "OP_MULTIPLY"),
            Divide => write!(f, "OP_DIVIDE"),
            Not => write!(f, "OP_NOT"),
            Negate => write!(f, "OP_NEGATE"),
            Print => write!(f, "OP_PRINT"),
            Return => write!(f, "OP_RETURN"),
        }
    }
}
