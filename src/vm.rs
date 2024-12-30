use std::fmt;

use crate::{chunk::Chunk, compiler::OpCode};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Null,
    String(String),
}

impl Value {
    pub fn as_number(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            _ => panic!("Expected number"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Null => write!(f, "null"),
            Value::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VmResult {
    OK,
    RuntimeError,
}

pub struct VM {
    stack: Vec<Value>,
    ip: usize,
    chunk: Chunk,
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            stack: Vec::new(),
            ip: 0,
            chunk,
        }
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Popped empty stack")
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.code[self.ip];
        self.ip += 1;
        byte
    }

    fn trace_execution(&self) {
        if !cfg!(feature = "debug_trace_execution") {
            return;
        }
        print!("          ");
        for slot in &self.stack {
            print!("[ {} ]", slot);
        }
        println!();
        self.chunk.disassemble_instruction(self.ip);
    }

    pub fn binary_op(&mut self, op: OpCode) {
        let b = self.pop().as_number();
        let a = self.pop().as_number();
        match op {
            OpCode::Add => self.push(Value::Number(a + b)),
            OpCode::Sub => self.push(Value::Number(a - b)),
            OpCode::Mul => self.push(Value::Number(a * b)),
            OpCode::Div => self.push(Value::Number(a / b)),
            _ => panic!("Unknown binary op"),
        }
    }

    pub fn run(&mut self) -> VmResult {
        loop {
            self.trace_execution();
            let instruction: OpCode = OpCode::from_u8(self.read_byte());
            match instruction {
                OpCode::Constant => {
                    let constant = self.read_byte();
                    let value = self.chunk.constants[constant as usize].clone();
                    self.push(value);
                }
                OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::Div => {
                    self.binary_op(instruction);
                }
                OpCode::Negate => {
                    let result = Value::Number(-self.pop().as_number());
                    self.push(result);
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::Print => println!("{}", self.pop()),
                OpCode::Return => {
                    return VmResult::OK;
                }
            }
        }
    }
}

pub fn interpret(source: &str) {
    use crate::compiler::compile;
    let compiled = compile(source);
    let chunk: Chunk = match compiled {
        crate::compiler::CompilerResult::Chunk(chunk) => chunk,
        crate::compiler::CompilerResult::CompileError => {
            eprintln!("Compile error");
            return;
        }
    };
    // TODO: disassemble chunks
    let mut vm = VM::new(chunk);
    let result = vm.run();
    match result {
        VmResult::OK => (),
        VmResult::RuntimeError => eprintln!("Runtime error"),
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::compile;

    use super::*;

    #[test]
    fn test_binary_ops() {
        for op in ["1 + 1;", "1 - 1;", "1 * 5;", "1 / 2;"].iter() {
            let compiled = compile(op);
            let chunk: Chunk = match compiled {
                crate::compiler::CompilerResult::Chunk(chunk) => chunk,
                crate::compiler::CompilerResult::CompileError => panic!("Compile error"),
            };
            let mut vm = VM::new(chunk);
            let result = vm.run();
            assert_eq!(result, VmResult::OK);
            assert_eq!(vm.stack.len(), 0);
        }
    }
}
