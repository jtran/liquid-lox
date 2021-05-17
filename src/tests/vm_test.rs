use std::mem;
use std::rc::Rc;
use num_traits::ToPrimitive;

use crate::compiler;
use crate::op::Chunk;
use crate::op::Op::*;
use crate::value::*;
use crate::value::Value::*;
use crate::vm::*;

fn eval(code: &str) -> Result<Value, RuntimeError> {
    let mut vm = Vm::new();
    let chunk = compiler::compile(code)?;

    vm.interpret_chunk(Rc::new(chunk))
}

fn eval_byte_code(code: Vec<u8>, constants: Vec<Value>) -> Result<Value, RuntimeError> {
    // Put everything on line 1.
    let lines = vec![1; code.len()];
    let mut chunk = Chunk::new(code, constants, lines);
    chunk.add_code_op(Return, 2);
    let mut vm = Vm::new();

    vm.interpret_chunk(Rc::new(chunk))
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
fn test_eval_literals() {
    assert_eq!(eval_byte_code(vec![Constant.to_u8().unwrap(), 0u8], vec![NumberVal(42.0)]), Ok(NumberVal(42.0)));
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
fn test_eval_binary_ops() {
    assert_eq!(eval("40 + 2"), Ok(NumberVal(42.0)));
    assert_eq!(eval("40.0 - 10"), Ok(NumberVal(30.0)));
    assert_eq!(eval("7 * 3"), Ok(NumberVal(21.0)));
    assert_eq!(eval("10 / 2"), Ok(NumberVal(5.0)));
    assert_eq!(eval("-(2)"), Ok(NumberVal(-2.0)));
}
