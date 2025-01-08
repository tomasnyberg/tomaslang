use std::{collections::HashMap, fmt};

use crate::{chunk::Chunk, compiler::Function, compiler::OpCode, compiler::Range};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Null,
    String(String),
    Function(Function),
    Range(Range),
    ReturnAddress(usize),
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

    pub fn as_function(&self) -> &Function {
        match self {
            Value::Function(f) => f,
            _ => panic!("Expected function"),
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Value::Number(n) => n.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Null => "null".to_string(),
            Value::String(s) => s.to_string(),
            Value::Function(f) => format!("<fn {} (starts at {})>", f.name.lexeme, f.start),
            Value::Range(r) => r.as_debug_string(),
            Value::ReturnAddress(x) => format!("RA {x}"),
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
    globals: HashMap<String, Value>,
    frame_starts: Vec<usize>,
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            stack: Vec::new(),
            ip: 0,
            chunk,
            had_runtime_error: false,
            globals: HashMap::new(),
            frame_starts: vec![0],
        }
    }

    pub fn load_chunk(&mut self, chunk: Chunk) {
        self.chunk = chunk;
        self.ip = 0;
        self.stack.clear();
    }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - distance]
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Popped empty stack")
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn insert(&mut self, value: Value, index: usize) {
        self.stack.insert(index, value);
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.code[self.ip];
        self.ip += 1;
        byte
    }

    fn local_idx_on_stack(&mut self) -> usize {
        let position = self.read_byte();
        position as usize + self.frame_starts.last().unwrap()
    }

    fn read_short(&mut self) -> u16 {
        let short = (self.chunk.code[self.ip] as u16) << 8 | self.chunk.code[self.ip + 1] as u16;
        self.ip += 2;
        short
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
                    let mut a = a.as_string();
                    let b = b.as_string();
                    a.push_str(&b);
                    self.push(Value::String(a));
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
            OpCode::Equal => self.push(Value::Bool(a == b)),
            OpCode::NotEqual => self.push(Value::Bool(a != b)),
            OpCode::Greater => {
                if a.is_string() && b.is_string() {
                    self.push(Value::Bool(a.as_string() > b.as_string()));
                    return;
                }
                if a.is_number() && b.is_number() {
                    self.push(Value::Bool(a.as_number() > b.as_number()));
                    return;
                }
                self.runtime_error(
                    "Values in comparison are not comparable (not two strings nor two numbers",
                );
            }
            OpCode::GreaterEqual => {
                if a.is_string() && b.is_string() {
                    self.push(Value::Bool(a.as_string() >= b.as_string()));
                    return;
                }
                if a.is_number() && b.is_number() {
                    self.push(Value::Bool(a.as_number() >= b.as_number()));
                    return;
                }
                self.runtime_error(
                    "Values in comparison are not comparable (not two strings nor two numbers",
                );
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
            Value::Function(_) => true,
            Value::Range(_) => true,
            Value::ReturnAddress(_) => {
                panic!("Return address should not be possible to evaluate for truth")
            }
        }
    }

    fn global_identifier(&mut self) -> String {
        let name_index = self.read_byte();
        let name = self.chunk.constants[name_index as usize].clone();
        if !name.is_string() {
            self.runtime_error("Expected string for global name");
        }
        name.as_string()
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
                OpCode::Add
                | OpCode::Sub
                | OpCode::Mul
                | OpCode::Div
                | OpCode::Equal
                | OpCode::NotEqual
                | OpCode::Greater
                | OpCode::GreaterEqual => {
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
                OpCode::Next => {
                    let range = self.pop();
                    // TODO PERF: validating this every iteration might be slow
                    let mut range = match range {
                        Value::Range(r) => r,
                        _ => {
                            self.runtime_error("Expected range");
                            return VmResult::RuntimeError;
                        }
                    };
                    let next = range.next();
                    if let Some(next) = next {
                        self.push(Value::Range(range));
                        self.push(Value::Number(next as f64));
                    } else {
                        self.push(Value::Null);
                    }
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefineGlobal => {
                    let name = self.global_identifier();
                    let value = self.pop();
                    self.globals.insert(name, value);
                }
                OpCode::GetGlobal => {
                    let name = self.global_identifier();
                    match self.globals.get(&name) {
                        Some(value) => self.push(value.clone()),
                        None => {
                            self.runtime_error(&format!("Undefined variable '{}'", name));
                            return VmResult::RuntimeError;
                        }
                    }
                }
                OpCode::SetGlobal => {
                    let name = self.global_identifier();
                    if !self.globals.contains_key(&name) {
                        self.runtime_error(&format!("Undefined variable '{}'", name));
                        return VmResult::RuntimeError;
                    }
                    let value = self.peek(0).clone();
                    self.globals.insert(name, value);
                }
                OpCode::GetLocal => {
                    let idx = self.local_idx_on_stack();
                    let value = self.stack[idx].clone();
                    self.push(value);
                }
                OpCode::SetLocal => {
                    let idx = self.local_idx_on_stack();
                    let value = self.peek(0).clone();
                    self.stack[idx] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short() as usize;
                    if !self.is_truthy(self.peek(0)) {
                        self.ip += offset;
                    }
                }
                OpCode::JumpIfTrue => {
                    let offset = self.read_short() as usize;
                    if self.is_truthy(self.peek(0)) {
                        self.ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short() as usize;
                    self.ip += offset;
                }
                OpCode::Loop => {
                    let offset = self.read_short() as usize;
                    self.ip -= offset;
                }
                OpCode::Call => {
                    let arg_c = self.pop().as_number() as usize;
                    let called = self.peek(arg_c);
                    let called_fn = match called {
                        Value::Function(_) => called.as_function(),
                        _ => {
                            self.runtime_error("Can only call functions");
                            return VmResult::RuntimeError;
                        }
                    };
                    if called_fn.arity != arg_c as u8 {
                        self.runtime_error(&format!(
                            "Expected {} arguments but got {}",
                            called_fn.arity, arg_c
                        ));
                        return VmResult::RuntimeError;
                    }
                    let return_address = Value::ReturnAddress(self.ip);
                    self.ip = called_fn.start;
                    // TODO PERF: don't really like the idea of inserting with an offset
                    self.insert(return_address, self.stack.len() - arg_c - 1);
                    self.frame_starts.push(self.stack.len() - arg_c - 1);
                }
                OpCode::Print => println!("{}", self.pop()),
                OpCode::Range => {
                    let end = self.pop().as_number() as i32;
                    let start = self.pop().as_number() as i32;
                    let range = Value::Range(Range::new(start, end));
                    self.push(range);
                }
                OpCode::Null => self.push(Value::Null),
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Return => {
                    let returned_value = self.pop();
                    let frame_start = self.frame_starts.pop().unwrap();
                    // Clear the functions locals
                    while self.stack.len() > frame_start {
                        self.pop();
                    }
                    let return_address = self.pop();
                    match return_address {
                        Value::ReturnAddress(ip) => self.ip = ip,
                        _ => {
                            self.runtime_error("Return did not pop a return address");
                            return VmResult::RuntimeError;
                        }
                    }
                    self.push(returned_value);
                }
                OpCode::Eof => {
                    return VmResult::OK;
                }
            }
            if self.had_runtime_error {
                return VmResult::RuntimeError;
            }
        }
    }
}

pub fn interpret(source: &str, vm: &mut VM) {
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
    vm.load_chunk(chunk);
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

    fn get_vm(program: &str) -> VM {
        let compiled = compile(program);
        let chunk: Chunk = match compiled {
            crate::compiler::CompilerResult::Chunk(chunk) => chunk,
            crate::compiler::CompilerResult::CompileError => panic!("Compile error"),
        };
        VM::new(chunk)
    }

    #[test]
    fn test_binary_ops() {
        for op in ["1 + 1;", "1 - 1;", "1 * 5;", "1 / 2;"].iter() {
            let mut vm = get_vm(op);
            let result = vm.run();
            assert_eq!(result, VmResult::OK);
            assert_eq!(vm.stack.len(), 0);
        }
    }

    #[test]
    fn global_variables() {
        let program = "global a = 1; print a; a = 5; print a;";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::OK);
        assert_eq!(vm.stack.len(), 0);
    }

    #[test]
    fn global_variables_errors() {
        let program = "b = 5;";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::RuntimeError);
        let program = "print b;";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::RuntimeError);
    }

    #[test]
    fn block_scoped() {
        let program = "let a = 1; { let a = 5; print a; }";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::OK);
    }

    #[test]
    fn comparison_operators() {
        let program = "1 > 2; 1 >= 2; 1 == 1; 1 != 2;";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::OK);
    }

    #[test]
    fn comparison_operators_bad() {
        let program = "1 > \"2\";";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::RuntimeError);
        let program = "\"hej\" >= 5;";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::RuntimeError);
    }

    #[test]
    fn function_calls() {
        let program = "fn f(a) { print a; } f(5);";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::OK);
        let program = "fn f(a) { print a; } f(5,10);";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::RuntimeError);
    }
}
