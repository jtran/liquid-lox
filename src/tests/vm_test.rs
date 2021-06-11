use std::mem;
use std::rc::Rc;
use num_traits::ToPrimitive;

use crate::compiler;
use crate::op::{Chunk, Op::*};
use crate::source_loc::SourceLoc;
use crate::value::*;
use crate::value::Value::*;
use crate::vm::*;

fn interpret(code: &str) -> Result<Value, RuntimeError> {
    let mut vm = Vm::new();
    let chunk = compiler::compile(code)?;

    vm.interpret_chunk(Rc::new(chunk))
}

fn eval(code: &str) -> Result<Value, RuntimeError> {
    // Get the expression value.
    let code = format!("return {};", code);
    interpret(&code)
}

fn eval_byte_code(code: Vec<u8>, constants: Vec<Value>) -> Result<Value, RuntimeError> {
    // Put everything on line 1.
    let lines = vec![1; code.len()];
    let mut chunk = Chunk::new(code, constants, lines);
    chunk.add_code_op(Return, 2);
    let mut vm = Vm::new();

    vm.interpret_chunk(Rc::new(chunk))
}

fn script_backtrace() -> Backtrace {
    Backtrace::new(Vec::new())
}

#[test]
fn test_vm_size_of_call_frame() {
    assert_eq!(mem::size_of::<CallFrame>(), 40);
}

#[test]
fn test_vm_size_of_internal_result() {
    assert_eq!(mem::size_of::<Result<Value, RuntimeError>>(), 32);
    assert_eq!(mem::size_of::<Result<Value, ExecutionInterrupt>>(), 40);
}

#[test]
fn test_eval_binary_ops_byte_code() {
    assert_eq!(eval_byte_code(vec![
        Constant.to_u8().unwrap(),
        0u8,
        Constant.to_u8().unwrap(),
        1u8,
        Add.to_u8().unwrap(),
    ],
    vec![NumberVal(40.0), NumberVal(2.0)]),
    Ok(NumberVal(42.0)));

    assert_eq!(eval_byte_code(vec![
        Constant.to_u8().unwrap(),
        0u8,
        Constant.to_u8().unwrap(),
        1u8,
        Subtract.to_u8().unwrap(),
    ],
    vec![NumberVal(40.0), NumberVal(10.0)]),
    Ok(NumberVal(30.0)));

    assert_eq!(eval_byte_code(vec![
        Constant.to_u8().unwrap(),
        0u8,
        Constant.to_u8().unwrap(),
        1u8,
        Multiply.to_u8().unwrap(),
    ],
    vec![NumberVal(7.0), NumberVal(3.0)]),
    Ok(NumberVal(21.0)));

    assert_eq!(eval_byte_code(vec![
        Constant.to_u8().unwrap(),
        0u8,
        Constant.to_u8().unwrap(),
        1u8,
        Divide.to_u8().unwrap(),
    ],
    vec![NumberVal(10.0), NumberVal(2.0)]),
    Ok(NumberVal(5.0)));
}

#[test]
fn test_eval_literals() {
    assert_eq!(eval("nil"), Ok(NilVal));
    assert_eq!(eval("true"), Ok(BoolVal(true)));
    assert_eq!(eval("false"), Ok(BoolVal(false)));
    assert_eq!(eval("42"), Ok(NumberVal(42.0)));
    assert_eq!(eval("\"hello\""), Ok(StringVal(Rc::new("hello".to_string()))));
}

#[test]
fn test_eval_binary_ops() {
    assert_eq!(eval("40 + 2"), Ok(NumberVal(42.0)));
    assert_eq!(eval("\"foo\" + \"bar\""), Ok(StringVal(Rc::new("foobar".to_string()))));
    assert_eq!(eval("40.0 - 10"), Ok(NumberVal(30.0)));
    assert_eq!(eval("7 * 3"), Ok(NumberVal(21.0)));
    assert_eq!(eval("10 / 2"), Ok(NumberVal(5.0)));
    assert_eq!(eval("nil + 1"), Err(RuntimeError::new(SourceLoc::new(1, 0), "Operands must be numbers.", script_backtrace())));
    assert_eq!(eval("1 - true"), Err(RuntimeError::new(SourceLoc::new(1, 0), "Operands must be numbers.", script_backtrace())));
    assert_eq!(eval("1 * false"), Err(RuntimeError::new(SourceLoc::new(1, 0), "Operands must be numbers.", script_backtrace())));
    assert_eq!(eval("1 / nil"), Err(RuntimeError::new(SourceLoc::new(1, 0), "Operands must be numbers.", script_backtrace())));
}

#[test]
fn test_eval_comparison() {
    assert_eq!(eval("true == true"), Ok(BoolVal(true)));
    assert_eq!(eval("true != true"), Ok(BoolVal(false)));
    assert_eq!(eval("true == 32"), Ok(BoolVal(false)));
    assert_eq!(eval("true != 32"), Ok(BoolVal(true)));
    assert_eq!(eval("nil == nil"), Ok(BoolVal(true)));
    assert_eq!(eval("nil != nil"), Ok(BoolVal(false)));
    assert_eq!(eval("nil == false"), Ok(BoolVal(false)));
    assert_eq!(eval("nil != false"), Ok(BoolVal(true)));
    assert_eq!(eval("\"foo\" == \"foo\""), Ok(BoolVal(true)));
    assert_eq!(eval("\"foo\" != \"foo\""), Ok(BoolVal(false)));
    assert_eq!(eval("\"foo\" == \"bar\""), Ok(BoolVal(false)));
    assert_eq!(eval("\"foo\" != \"bar\""), Ok(BoolVal(true)));
    assert_eq!(eval("2 < 3"), Ok(BoolVal(true)));
    assert_eq!(eval("2 > 3"), Ok(BoolVal(false)));
    assert_eq!(eval("2 <= 3"), Ok(BoolVal(true)));
    assert_eq!(eval("2 >= 3"), Ok(BoolVal(false)));
    assert_eq!(eval("nil < 1"), Err(RuntimeError::new(SourceLoc::new(1, 0), "Operands must be numbers.", script_backtrace())));
    assert_eq!(eval("1 > true"), Err(RuntimeError::new(SourceLoc::new(1, 0), "Operands must be numbers.", script_backtrace())));
    assert_eq!(eval("1 <= false"), Err(RuntimeError::new(SourceLoc::new(1, 0), "Operands must be numbers.", script_backtrace())));
    assert_eq!(eval("1 >= nil"), Err(RuntimeError::new(SourceLoc::new(1, 0), "Operands must be numbers.", script_backtrace())));
}

#[test]
fn test_eval_unary_ops() {
    assert_eq!(eval("!true"), Ok(BoolVal(false)));
    assert_eq!(eval("!false"), Ok(BoolVal(true)));
    assert_eq!(eval("!nil"), Ok(BoolVal(true)));
    assert_eq!(eval("!0"), Ok(BoolVal(false)));
    assert_eq!(eval("!\"\""), Ok(BoolVal(false)));
    assert_eq!(eval("-(2)"), Ok(NumberVal(-2.0)));
    assert_eq!(eval("-true"), Err(RuntimeError::new(SourceLoc::new(1, 0), "Operand must be a number.", script_backtrace())));
}

#[test]
fn test_print() {
    assert_eq!(interpret("print \"print test\";"), Ok(NilVal));
    assert_eq!(interpret("print 1 + 2;"), Ok(NilVal));
    assert_eq!(interpret("print 1"), Err(RuntimeError::new(SourceLoc::new(1, 8), "parse error: Expect ';' after value.",
        Backtrace::new(vec![BacktraceItem::new("(parser)".to_string(), SourceLoc::new(1, 1))]))));
}
