use crate::RuntimeError;
use crate::op::{Chunk, Op};
use crate::value::Value;

const FRAMES_MAX: usize = 64;

const U8_COUNT: usize = std::u8::MAX as usize + 1;
const STACK_MAX: usize = FRAMES_MAX * U8_COUNT;

#[derive(Debug)]
pub struct Vm<'chunk> {
    frames: Vec<CallFrame<'chunk>>,
    stack: Vec<Value>,
}

#[derive(Debug)]
pub struct CallFrame<'chunk> {
    chunk: &'chunk Chunk,
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

impl<'chunk> Vm<'chunk> {
    pub fn new() -> Vm<'chunk> {
        Vm {
            frames: Vec::with_capacity(FRAMES_MAX),
            stack: Vec::with_capacity(STACK_MAX),
        }
    }

    pub fn interpret_chunk(&mut self, chunk: &'chunk Chunk) -> Result<Value, RuntimeError> {
        // Our one and only call frame for now.
        self.frames.push(CallFrame::new(chunk));

        self.run()
    }

    fn run(&mut self) -> Result<Value, RuntimeError> {
        let frames_len = self.frames.len();
        let mut frame = &mut self.frames[frames_len - 1];
        loop {
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
                    let y = pop!(self);
                    let x = pop!(self);
                    match (x, y) {
                        (Value::NumberVal(x), Value::NumberVal(y)) => {
                            self.stack.push(Value::NumberVal(x + y));
                        }
                        (x, y) => panic!("Wrong type for plus op: {:?} + {:?}", x, y),
                    }
                },
                Op::Subtract => {
                    let y = pop!(self);
                    let x = pop!(self);
                    match (x, y) {
                        (Value::NumberVal(x), Value::NumberVal(y)) => {
                            self.stack.push(Value::NumberVal(x - y));
                        }
                        (x, y) => panic!("Wrong type for minus op: {:?} - {:?}", x, y),
                    }
                },
                Op::Multiply => {
                    let y = pop!(self);
                    let x = pop!(self);
                    match (x, y) {
                        (Value::NumberVal(x), Value::NumberVal(y)) => {
                            self.stack.push(Value::NumberVal(x * y));
                        }
                        (x, y) => panic!("Wrong type for multiply op: {:?} * {:?}", x, y),
                    }
                },
                Op::Divide => {
                    let y = pop!(self);
                    let x = pop!(self);
                    match (x, y) {
                        (Value::NumberVal(x), Value::NumberVal(y)) => {
                            self.stack.push(Value::NumberVal(x / y));
                        }
                        (x, y) => panic!("Wrong type for divide op: {:?} / {:?}", x, y),
                    }
                },
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

impl<'chunk> CallFrame<'chunk> {
    pub fn new(chunk: &'chunk Chunk) -> CallFrame<'chunk> {
        CallFrame {
            chunk: chunk,
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

    pub fn get_op(&self, ip: usize) -> Option<Op> {
        self.chunk.get_op_code(ip)
    }

    pub fn code_byte(&self, ip: usize) -> u8 {
        self.chunk.code_byte(ip)
    }

    pub fn constant(&self, index: usize) -> Value {
        self.chunk.constant(index)
    }
}
