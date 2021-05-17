use std::ops::{Add, Div, Mul, Sub};
use std::rc::Rc;

use crate::compiler::U8_COUNT;
use crate::RuntimeError;
use crate::op::{Chunk, Op};
use crate::value::Value;

const FRAMES_MAX: usize = 64;

const STACK_MAX: usize = FRAMES_MAX * U8_COUNT;

#[derive(Debug)]
pub struct Vm {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
}

type ChunkRef = Rc<Chunk>;

#[derive(Debug)]
pub struct CallFrame {
    chunk: ChunkRef,
    ip: usize,
    slots: Vec<Value>,
}

// Pop from the stack.  This is a macro instead of a function since the rust
// compiler isn't smart enough to track which fields of a struct get borrowed
// through a function call.  It treats the entire self as borrowed.  But when
// it's inline, the specific fields are transparent.
macro_rules! pop {
    ( $x:expr ) => {
        $x.stack.pop().expect("popped past the end of the stack")
    };
}

// Binary op for numeric operations.
macro_rules! number_bin_op {
    ( $self:expr, $op:ident, $op_str:expr, $op_name:expr ) => {
        let y = pop!($self);
        let x = pop!($self);
        match (x, y) {
            (Value::NumberVal(x), Value::NumberVal(y)) => {
                $self.stack.push(Value::NumberVal(x.$op(y)));
            }
            (x, y) => panic!("Wrong type for {} op: {:?} {} {:?}", $op_name, x, $op_str, y),
        }
    };
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            frames: Vec::with_capacity(FRAMES_MAX),
            stack: Vec::with_capacity(STACK_MAX),
        }
    }

    pub fn interpret_chunk(&mut self, chunk: ChunkRef) -> Result<Value, RuntimeError> {
        // Our one and only call frame for now.
        self.frames.push(CallFrame::new(chunk));

        self.run()
    }

    fn run(&mut self) -> Result<Value, RuntimeError> {
        let frames_len = self.frames.len();
        let mut frame = &mut self.frames[frames_len - 1];
        loop {
            #[cfg(feature = "debug-trace-execution")]
            frame.chunk.disassemble_instruction(frame.ip);

            let op = frame.get_op(frame.ip).unwrap_or_else(||
                panic!("unknown op code: {:?}", frame.code_byte(frame.ip))
            );
            frame.ip += 1;
            match op {
                Op::Constant => {
                    self.stack.push(frame.constant(usize::from(frame.peek_byte())));
                    frame.inc_ip();
                },
                Op::Add => {
                    number_bin_op!(self, add, "+", "plus");
                },
                Op::Subtract => {
                    number_bin_op!(self, sub, "-", "minus");
                },
                Op::Multiply => {
                    number_bin_op!(self, mul, "*", "multiply");
                },
                Op::Divide => {
                    number_bin_op!(self, div, "/", "divide");
                },
                Op::Negate => {
                    let x = pop!(self);
                    match x {
                        Value::NumberVal(x) => {
                            self.stack.push(Value::NumberVal(-x));
                        }
                        _ => panic!("Wrong type for negate op: -{:?}", x),
                    }
                }
                Op::Print => {
                    print_value(pop!(self));
                },
                Op::Return => {
                    return Ok(pop!(self));
                }
            }
        }
    }
}

fn print_value(value: Value) {
    println!("{}", value.to_runtime_string());
}

impl CallFrame {
    pub fn new(chunk: ChunkRef) -> CallFrame {
        CallFrame {
            chunk,
            ip: 0,
            slots: Vec::default(),
        }
    }

    #[inline]
    fn peek_byte(&self) -> u8 {
        self.code_byte(self.ip)
    }

    #[inline]
    fn inc_ip(&mut self) {
        self.ip += 1;
    }

    pub fn get_op(&self, index: usize) -> Option<Op> {
        self.chunk.get_op_code(index)
    }

    pub fn code_byte(&self, index: usize) -> u8 {
        self.chunk.code_byte(index)
    }

    pub fn constant(&self, index: usize) -> Value {
        self.chunk.constant(index)
    }
}
