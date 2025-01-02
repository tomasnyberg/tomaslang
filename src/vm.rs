use std::fmt;

use crate::{chunk::Chunk, compiler::OpCode};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Null,
    String(String),
}

#[allow(dead_code)]
impl Value {
    pub fn as_number(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            // TODO: Not sure if this should behave differently for other types?
            _ => panic!("Expected number"),
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Value::Number(n) => n.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Null => "null".to_string(),
            Value::String(s) => s.clone(),
        }
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_string())
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
    had_runtime_error: bool,
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            stack: Vec::new(),
            ip: 0,
            chunk,
            had_runtime_error: false,
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
        if !self.stack.is_empty() {
            print!("          ");
            for slot in &self.stack {
                print!("[ {} ]", slot);
            }
        }
        println!();
        self.chunk.disassemble_instruction(self.ip);
    }

    fn runtime_error(&mut self, message: &str) {
        eprint!("{}", message);
        eprintln!(" at [line {}] in script", self.chunk.lines[self.ip - 1]);
        self.had_runtime_error = true;
    }

    fn binary_op(&mut self, op: OpCode) {
        let b: Value = self.pop();
        let a: Value = self.pop();
        match op {
            OpCode::Add => {
                if a.is_string() || b.is_string() {
                    let a = a.as_string();
                    let b = b.as_string();
                    self.push(Value::String(format!("{}{}", a, b)));
                    return;
                }
                self.push(Value::Number(a.as_number() + b.as_number()));
            }
            OpCode::Sub => {
                if !a.is_number() || !b.is_number() {
                    self.runtime_error("Expected numbers to sub operation");
                    return;
                }
                self.push(Value::Number(a.as_number() - b.as_number()));
            }
            OpCode::Mul => {
                if a.is_string() && b.is_number() {
                    let (a, b) = (a.as_string(), b.as_number());
                    self.push(Value::String(a.repeat(b as usize)));
                    return;
                }
                if !a.is_number() || !b.is_number() {
                    self.runtime_error("Bad operators to mul (*)");
                    return;
                }
                self.push(Value::Number(a.as_number() * b.as_number()));
            }
            OpCode::Div => {
                if a.is_string() || b.is_string() {
                    self.runtime_error("Cannot divide strings");
                    return;
                }
                self.push(Value::Number(a.as_number() / b.as_number()));
            }
            _ => panic!("Unknown binary op"),
        }
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Bool(b) => *b,
            Value::Null => false,
            Value::Number(n) => *n != 0.0,
            Value::String(s) => !s.is_empty(),
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
                    let value: Value = self.pop();
                    match value {
                        Value::Number(n) => self.push(Value::Number(-n)),
                        _ => {
                            self.runtime_error("Expected number");
                        }
                    }
                }
                OpCode::Not => {
                    let value = self.pop();
                    let result = Value::Bool(!self.is_truthy(&value));
                    self.push(result);
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefineGlobal => {
                    unimplemented!("DefineGlobal");
                }
                OpCode::GetGlobal => {
                    unimplemented!("GetGlobal");
                }
                OpCode::Print => println!("{}", self.pop()),
                OpCode::Null => self.push(Value::Null),
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Return => {
                    return VmResult::OK;
                }
            }
            if self.had_runtime_error {
                return VmResult::RuntimeError;
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
    chunk.disassemble("main");
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
