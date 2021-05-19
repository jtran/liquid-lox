use std::ops::{Add, Div, Mul, Sub};
use std::rc::Rc;

use crate::compiler::U8_COUNT;
use crate::op::{Chunk, Op};
use crate::source_loc::*;
use crate::value::{Backtrace, RuntimeError, Value};

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
    ( $self:expr ) => {
        $self.stack.pop().expect("popped past the end of the stack")
    };
}

// Binary op for numeric operations.
macro_rules! number_bin_op {
    ( $self:expr, $frame:expr, $cur_op_index:expr, $op:ident ) => {
        let y = pop!($self);
        let x = pop!($self);
        match (x, y) {
            (Value::NumberVal(x), Value::NumberVal(y)) => {
                $self.stack.push(Value::NumberVal(x.$op(y)));
            }
            (x, y) => {
                $self.stack.push(x);
                $self.stack.push(y);
                return Err(RuntimeError::new(SourceLoc::new(
                    $frame.chunk.line($cur_op_index), 0),
                    "Operands must be numbers.",
                    $self.backtrace()));
            }
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

        let result = self.run();

        if result.is_err() {
            self.reset_stack();
        }

        result
    }

    fn run(&mut self) -> Result<Value, RuntimeError> {
        let frames_len = self.frames.len();
        let mut frame = &mut self.frames[frames_len - 1];
        loop {
            let cur_op_index = frame.ip;

            #[cfg(feature = "debug-trace-execution")]
            frame.chunk.disassemble_instruction(cur_op_index);

            let op = frame.get_op(cur_op_index).unwrap_or_else(||
                panic!("unknown op code: {:?}", frame.code_byte(cur_op_index))
            );
            frame.ip += 1;
            match op {
                Op::Constant => {
                    self.stack.push(frame.constant(usize::from(frame.peek_byte())));
                    frame.inc_ip();
                }
                Op::Nil => {
                    self.stack.push(Value::NilVal);
                }
                Op::True => {
                    self.stack.push(Value::BoolVal(true));
                }
                Op::False => {
                    self.stack.push(Value::BoolVal(false));
                }
                Op::Add => {
                    number_bin_op!(self, frame, cur_op_index, add);
                }
                Op::Subtract => {
                    number_bin_op!(self, frame, cur_op_index, sub);
                }
                Op::Multiply => {
                    number_bin_op!(self, frame, cur_op_index, mul);
                }
                Op::Divide => {
                    number_bin_op!(self, frame, cur_op_index, div);
                }
                Op::Not => {
                    let v = pop!(self);
                    self.stack.push(Value::BoolVal(!v.is_truthy()));
                }
                Op::Negate => {
                    match pop!(self) {
                        Value::NumberVal(x) => {
                            self.stack.push(Value::NumberVal(-x));
                        }
                        val => {
                            self.stack.push(val);
                            return Err(RuntimeError::new(SourceLoc::new(
                                frame.chunk.line(cur_op_index), 0),
                                "Operand must be a number.",
                                self.backtrace()));
                        }
                    }
                }
                Op::Print => {
                    print_value(pop!(self));
                }
                Op::Return => {
                    return Ok(pop!(self));
                }
            }
        }
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
        self.frames.clear();
    }

    fn backtrace(&self) -> Backtrace {
        // TODO
        Backtrace::new(Vec::new())
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
