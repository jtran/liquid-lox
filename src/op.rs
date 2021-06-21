use num_traits::{FromPrimitive, ToPrimitive};
use num_derive::{FromPrimitive, ToPrimitive};
use crate::Value;

#[derive(Debug, Copy, Clone, FromPrimitive, ToPrimitive)]
pub enum Op {
    Constant,
    Nil,
    True,
    False,

    Pop,

    GetLocal,
    SetLocal,

    GetGlobal,
    DefineGlobal,
    SetGlobal,

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

    Jump,
    JumpIfFalse,

    Return,
}

#[derive(Debug)]
pub struct Uninterned {
    pub value: Value,
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

    pub fn code_len(&self) -> usize {
        self.code.len()
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

    pub fn set_code_byte(&mut self, index: usize, byte: u8) {
        self.code[index] = byte;
    }

    pub fn constant(&self, index: usize) -> Uninterned {
        Uninterned { value: self.constants[index].clone() }
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
            Some(op @ Op::Pop) => self.simple_instruction(op, offset),
            Some(op @ Op::GetLocal) |
            Some(op @ Op::SetLocal) => self.byte_instruction(op, offset),
            Some(op @ Op::GetGlobal) |
            Some(op @ Op::DefineGlobal) |
            Some(op @ Op::SetGlobal) => self.constant_instruction(op, offset),
            Some(op @ Op::Equal) |
            Some(op @ Op::Greater) |
            Some(op @ Op::Less) |
            Some(op @ Op::Add) |
            Some(op @ Op::Subtract) |
            Some(op @ Op::Multiply) |
            Some(op @ Op::Divide) |
            Some(op @ Op::Not) |
            Some(op @ Op::Negate) |
            Some(op @ Op::Print) => self.simple_instruction(op, offset),
            Some(op @ Op::Jump) |
            Some(op @ Op::JumpIfFalse) => self.jump_instruction(op, 1, offset),
            Some(op @ Op::Return) => self.simple_instruction(op, offset),
        }
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

    fn simple_instruction(&self, op: Op, offset: usize) -> usize {
        println!("{}", op);

        offset + 1
    }

    fn byte_instruction(&self, op: Op, offset: usize) -> usize {
        let byte = self.code[offset + 1];
        println!("{:<16} {:>4}", op, byte);

        offset + 2
    }

    fn jump_instruction(&self, op: Op, sign: u16, offset: usize) -> usize {
        let jump_delta = u16::from(self.code[offset + 1]) << 8
            | u16::from(self.code[offset + 2]);
        println!("{:<16} {:>4} -> {}",
                 op,
                 jump_delta,
                 offset + 3 + usize::from(sign * jump_delta));

        offset + 3
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
            Pop => write!(f, "OP_POP"),
            GetLocal => write!(f, "OP_GET_LOCAL"),
            SetLocal => write!(f, "OP_SET_LOCAL"),
            GetGlobal => write!(f, "OP_GET_GLOBAL"),
            DefineGlobal => write!(f, "OP_DEFINE_GLOBAL"),
            SetGlobal => write!(f, "OP_SET_GLOBAL"),
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
            Jump => write!(f, "OP_JUMP"),
            JumpIfFalse => write!(f, "OP_JUMP_IF_FALSE"),
            Return => write!(f, "OP_RETURN"),
        }
    }
}
