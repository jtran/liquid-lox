use std::rc::Rc;

use fnv::FnvHashMap;

use crate::compiler::U8_COUNT;
use crate::op::{Chunk, Op, Uninterned};
use crate::source_loc::*;
use crate::value::{Backtrace, RuntimeError, Value};

const FRAMES_MAX: usize = 64;

const STACK_MAX: usize = FRAMES_MAX * U8_COUNT;

#[derive(Debug)]
pub struct Vm {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    // Interned strings.  We can switch to HashSet once hash_set_entry lands in
    // stable Rust.
    strings: FnvHashMap<Rc<String>, Rc<String>>,
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

// Push onto the stack.  This is just for convenience.
macro_rules! push {
    ( $self:expr, $value:expr ) => {
        $self.stack.push($value)
    };
}

// Binary op for numeric operations.
macro_rules! bin_op {
    ( $self:expr, $frame:expr, $cur_op_index:expr, $val_constr:ident, $op:tt ) => {
        let y = pop!($self);
        let x = pop!($self);
        match (x, y) {
            (Value::NumberVal(x), Value::NumberVal(y)) => {
                $self.stack.push(Value::$val_constr(x $op y));
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

// Intern a String and return the interned Value.
macro_rules! intern_string {
    ( $self:expr, $s:expr ) => {{
        let s = Rc::new($s);
        let interned = $self.strings.entry(Rc::clone(&s)).or_insert(s);
        Value::StringVal(Rc::clone(interned))
    }};
}

// Intern an Uninterned and return the interned Value.  For Values other than
// string, this just unwraps the Uninterned to get its underlying Value.
macro_rules! intern {
    ( $self:expr, $uninterned:expr ) => {{
        let un: Uninterned = $uninterned;
        match un.value {
            Value::StringVal(s) => {
                let interned = $self.strings.entry(Rc::clone(&s)).or_insert(s);
                Value::StringVal(Rc::clone(interned))
            }
            v => v,
        }
    }};
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            frames: Vec::with_capacity(FRAMES_MAX),
            stack: Vec::with_capacity(STACK_MAX),
            strings: FnvHashMap::with_capacity_and_hasher(64, Default::default()),
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
                    let constant = frame.constant(usize::from(frame.peek_byte()));
                    self.stack.push(intern!(self, constant));
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
                Op::Equal => {
                    let y = pop!(self);
                    let x = pop!(self);
                    self.stack.push(Value::BoolVal(x.is_equal_interned(&y)));
                }
                Op::Greater => {
                    bin_op!(self, frame, cur_op_index, BoolVal, >);
                }
                Op::Less => {
                    bin_op!(self, frame, cur_op_index, BoolVal, <);
                }
                Op::Add => {
                    let y = pop!(self);
                    let x = pop!(self);
                    match (x, y) {
                        (Value::StringVal(x), Value::StringVal(y)) => {
                            let s = format!("{}{}", *x, *y);
                            push!(self, intern_string!(self, s));
                        }
                        (Value::NumberVal(x), Value::NumberVal(y)) => {
                            push!(self, Value::NumberVal(x + y));
                        }
                        (x, y) => {
                            push!(self, x);
                            push!(self, y);
                            return Err(RuntimeError::new(SourceLoc::new(
                                frame.chunk.line(cur_op_index), 0),
                                "Operands must be numbers.",
                                self.backtrace()));
                        }
                    }
                }
                Op::Subtract => {
                    bin_op!(self, frame, cur_op_index, NumberVal, -);
                }
                Op::Multiply => {
                    bin_op!(self, frame, cur_op_index, NumberVal, *);
                }
                Op::Divide => {
                    bin_op!(self, frame, cur_op_index, NumberVal, /);
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

    pub fn constant(&self, index: usize) -> Uninterned {
        self.chunk.constant(index)
    }
}
