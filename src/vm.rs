use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{
    chunk::Chunk, compiler::Function, compiler::Iterable, compiler::Iterator, compiler::OpCode,
    compiler::Range,
};

#[derive(Debug, Clone, Eq)]
pub struct CiggHashMap(HashMap<Value, Value>);

impl PartialEq for CiggHashMap {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Null,
    String(Rc<RefCell<Vec<char>>>),
    Function(Function),
    Range(Range),
    Iterable(Box<dyn Iterable>),
    Array(Rc<RefCell<Vec<Value>>>),
    HashMap(Rc<RefCell<CiggHashMap>>),
    ReturnAddress(usize),
    NativeFn(NativeFn),
}

// Needed for type acrobatics with Iterable
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Null, Value::Null) => true,
            (Value::String(a), Value::String(b)) => *a.borrow() == *b.borrow(),
            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::Range(a), Value::Range(b)) => a == b,
            (Value::Iterable(a), Value::Iterable(b)) => a.eq(b),
            (Value::Array(a), Value::Array(b)) => *a.borrow() == *b.borrow(),
            (Value::HashMap(a), Value::HashMap(b)) => *a.borrow() == *b.borrow(),
            (Value::ReturnAddress(a), Value::ReturnAddress(b)) => a == b,
            (Value::NativeFn(a), Value::NativeFn(b)) => a == b,
            _ => false,
        }
    }
}

// NOTE: this might not hold up perfectly
impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Tag each variant (the x_u8 part) distinctly so they do not collide
        match self {
            Value::Number(n) => {
                0_u8.hash(state);
                n.to_bits().hash(state);
            }
            Value::Bool(b) => {
                1_u8.hash(state);
                b.hash(state);
            }
            Value::Null => {
                2_u8.hash(state);
            }
            Value::String(s) => {
                3_u8.hash(state);
                let borrowed = s.borrow();
                Hash::hash_slice(&borrowed, state);
            }
            Value::Array(arr) => {
                4_u8.hash(state);
                let borrowed = arr.borrow();
                for val in &*borrowed {
                    val.hash(state);
                }
            }
            Value::Function(_) => {
                unimplemented!()
            }
            Value::Range(_) => {
                unimplemented!()
            }
            Value::Iterable(_) => {
                unimplemented!()
            }
            Value::HashMap(_) => {
                unimplemented!()
            }
            Value::ReturnAddress(_) => {
                unimplemented!()
            }
            Value::NativeFn(_) => {
                unimplemented!()
            }
        }
    }
}

// NOTE: Comparing values not of the types below will give weird results, but I decided
// I don't really care about it. Rust does care however so disable that linting message.
#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Value::*;
        match (self, other) {
            (Number(a), Number(b)) => a.partial_cmp(b),
            (Bool(a), Bool(b)) => Some(a.cmp(b)),
            (Null, Null) => Some(Ordering::Equal),
            (String(a), String(b)) => Some(a.borrow().cmp(&*b.borrow())),
            (Array(a), Array(b)) => Some(a.borrow().cmp(&*b.borrow())),
            (ReturnAddress(a), ReturnAddress(b)) => Some(a.cmp(b)),
            _ => None,
        }
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Value {
    pub fn as_number(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            // TODO: Not sure if this should behave differently for other types?
            _ => {
                let panic_message = format!("Expected number, got {:?}", self);
                panic!("{}", panic_message);
            }
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Value::Number(n) => n.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Null => "null".to_string(),
            Value::String(s) => s.borrow().iter().collect(),
            Value::Function(f) => format!("<fn {} (starts at {})>", f.name.lexeme, f.start),
            Value::Range(r) => r.as_debug_string(),
            Value::Iterable(_) => "iterable".to_string(),
            Value::Array(a) => {
                let a = a.borrow();
                let mut result = String::from("[");
                for (i, item) in a.iter().enumerate() {
                    result.push_str(&item.as_string());
                    if i < a.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push(']');
                result
            }
            Value::HashMap(m) => {
                let m = m.borrow();
                let mut result = String::from("{");
                for (key, value) in m.0.iter() {
                    result.push_str(&format!("{}: {}", key.as_string(), value.as_string()));
                    result.push_str(", ");
                }
                result.pop();
                result.pop();
                result.push('}');
                result
            }
            Value::ReturnAddress(x) => format!("RA {x}"),
            Value::NativeFn(n) => format!("<native fn {}>", n.name),
        }
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_range(&self) -> bool {
        matches!(self, Value::Range(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Value::Array(_))
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Null => false,
            Value::Number(n) => *n != 0.0,
            Value::String(s) => !s.borrow().is_empty(),
            Value::Function(_) => true,
            Value::Range(_) => true,
            Value::Iterable(_) => true,
            Value::Array(a) => !a.borrow().is_empty(),
            Value::HashMap(_) => true,
            Value::ReturnAddress(_) => {
                panic!("Return address should not be possible to evaluate for truth")
            }
            Value::NativeFn(_) => true,
        }
    }

    pub fn check_index_inbounds<T>(&self, index: &Value, target: &[T]) -> Option<String> {
        let target_type = match self {
            Value::Array(_) => "array",
            Value::String(_) => "string",
            _ => unreachable!(),
        };
        match index {
            Value::Number(n) => {
                let index = *n as usize;
                if index >= target.len() {
                    return Some(format!("Index {} out of bounds for {}", index, target_type));
                }
                None
            }
            Value::Range(r) => {
                let low = r.start;
                let high = r.end;
                if (low as f64) < 0.0
                    || (high as f64) < 0.0
                    || low as f64 > target.len() as f64
                    || (high as f64) > target.len() as f64
                {
                    Some(format!(
                        "Range {}..{} out of bounds for {}",
                        low, high, target_type
                    ))
                } else {
                    None
                }
            }
            _ => Some(format!("Expected number, got {}", index)),
        }
    }

    pub fn check_valid_access(&self, index: &Value, is_set: bool) -> Option<String> {
        if is_set && index.is_range() {
            return Some("Cannot set with range".to_string());
        }
        match self {
            Value::Array(a) => self.check_index_inbounds(index, &a.borrow()),
            Value::String(s) => self.check_index_inbounds(index, &s.borrow()),
            Value::HashMap(m) => {
                if index.is_range() {
                    return Some("Cannot access hashmap with range".to_string());
                }
                if is_set {
                    return None;
                }
                let m = m.borrow();
                if !m.0.contains_key(index) {
                    return Some(format!("Key not found in hashmap: {}", index));
                }
                None
            }
            _ => unreachable!(),
        }
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
    internally_running_fns: usize,
    globals: HashMap<String, Value>,
    frame_starts: Vec<usize>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct NativeFn {
    name: String,
    arity: u8,
    function: fn(&mut VM, &[Value]) -> Value,
}

impl NativeFn {
    fn new(name: &str, arity: u8, function: fn(&mut VM, &[Value]) -> Value) -> Self {
        Self {
            name: name.to_string(),
            arity,
            function,
        }
    }
}

pub struct TransformationFunction {
    pub name: &'static str,
    function: fn(&mut VM),
}

fn apply_function(vm: &mut VM, function: Value, item: Value) -> Value {
    match function {
        Value::Function(_) => {
            vm.push(function.clone());
            vm.push(item.clone());
            vm.execute_cigg_function(1)
        }
        Value::NativeFn(ref nf) => {
            let args = vec![item.clone()];
            (nf.function)(vm, &args)
        }
        _ => {
            vm.runtime_error("Expected function or native function");
            Value::Null
        }
    }
}

fn map(vm: &mut VM) {
    let array = vm.pop();
    let function = vm.pop();

    let mut result = Vec::new();
    match array {
        Value::Array(a) => {
            let a = a.borrow();
            for item in a.iter() {
                let mapped = apply_function(vm, function.clone(), item.clone());
                result.push(mapped);
            }
            vm.push(Value::Array(Rc::new(RefCell::new(result))));
        }
        _ => {
            vm.runtime_error("Expected array for map");
        }
    }
}

fn filter(vm: &mut VM) {
    let array = vm.pop();
    let function = vm.pop();

    let mut result = Vec::new();
    match array {
        Value::Array(a) => {
            let a = a.borrow();
            for item in a.iter() {
                let passed_filter = apply_function(vm, function.clone(), item.clone()).is_truthy();
                if passed_filter {
                    result.push(item.clone());
                }
            }
            vm.push(Value::Array(Rc::new(RefCell::new(result))));
        }
        _ => {
            vm.runtime_error("Expected array for filter");
        }
    }
}

fn words_delimiter(vm: &mut VM) {
    let string = vm.pop();
    let delimiter = vm.pop();
    words(vm, string, delimiter);
}

fn words_simple(vm: &mut VM) {
    let space = Value::String(Rc::new(RefCell::new(" ".chars().collect())));
    let string = vm.pop();
    words(vm, string, space);
}

fn unescape(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(next) = chars.next() {
                match next {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    '\\' => result.push('\\'),
                    other => {
                        result.push(ch);
                        result.push(other);
                    }
                }
            } else {
                result.push(ch);
            }
        } else {
            result.push(ch);
        }
    }
    result
}

fn words(vm: &mut VM, string: Value, delimiter: Value) {
    if !string.is_string() || !delimiter.is_string() {
        vm.runtime_error("Expected string as input and delimiter for words");
        return;
    }
    let string = string.as_string();
    let delimiter = delimiter.as_string();
    // Unescape the delimiter so that e.g. "\n" becomes an actual newline
    let delimiter = unescape(&delimiter);
    let words_iter = string.split(&delimiter);
    let words: Vec<Value> = words_iter
        .map(|word| Value::String(Rc::new(RefCell::new(word.chars().collect()))))
        .collect();
    vm.push(Value::Array(Rc::new(RefCell::new(words))));
}

fn sort(vm: &mut VM) {
    let array = vm.pop();
    match array {
        Value::Array(a) => {
            let mut a = a.borrow_mut().clone();
            a.sort();
            vm.push(Value::Array(Rc::new(RefCell::new(a))));
        }
        _ => {
            vm.runtime_error("Expected array for sort");
        }
    }
}

pub const TRANSFORMATION_FNS: [TransformationFunction; 5] = [
    TransformationFunction {
        name: "map",
        function: map,
    },
    TransformationFunction {
        name: "filter",
        function: filter,
    },
    TransformationFunction {
        name: "words",
        function: words_delimiter,
    },
    // NOTE: Has to be right after words since the compiler assumes so.
    TransformationFunction {
        name: "words_simple",
        function: words_simple,
    },
    TransformationFunction {
        name: "sort",
        function: sort,
    },
];

impl VM {
    fn add_native_fn(&mut self, name: &str, arity: u8, function: fn(&mut VM, &[Value]) -> Value) {
        let native_fn = NativeFn::new(name, arity, function);
        let value = Value::NativeFn(native_fn);
        self.globals.insert(name.to_string(), value);
    }

    fn fold_numbers<F>(vm: &mut VM, target: &Value, identity: f64, op: F, func_name: &str) -> Value
    where
        F: Fn(f64, f64) -> f64,
    {
        match target {
            Value::Array(a) => {
                let array = a.borrow();
                if !array.iter().all(|x| x.is_number()) {
                    vm.runtime_error(&format!("Expected array of numbers for {}", func_name));
                    return Value::Null;
                }
                let result = array.iter().map(|x| x.as_number()).fold(identity, op);
                if result == identity {
                    vm.runtime_error(&format!("Empty array for {}", func_name));
                    return Value::Null;
                }
                Value::Number(result)
            }
            _ => {
                vm.runtime_error(&format!("Expected array of numbers for {}", func_name));
                Value::Null
            }
        }
    }

    fn add_native_fns(&mut self) {
        self.add_native_fn("print", 1, |_vm_ref, args| {
            println!("{}", args[0]);
            Value::Null
        });
        self.add_native_fn("read_file", 1, |vm_ref, args| {
            let filename = args[0].as_string();
            if !(std::path::Path::new(&filename).exists()
                && std::path::Path::new(&filename).is_file())
            {
                vm_ref.runtime_error(&format!("File not found: {}", filename));
                return Value::Null;
            }
            let contents = std::fs::read_to_string(filename).unwrap_or_else(|_| "".to_string());
            Value::String(Rc::new(RefCell::new(contents.chars().collect())))
        });
        self.add_native_fn("len", 1, |vm_ref, args| {
            let target = &args[0];
            match target {
                Value::Array(a) => Value::Number(a.borrow().len() as f64),
                Value::String(s) => Value::Number(s.borrow().len() as f64),
                Value::HashMap(m) => Value::Number(m.borrow().0.len() as f64),
                _ => {
                    vm_ref.runtime_error("Expected array, string or hashmap for len");
                    Value::Null
                }
            }
        });
        self.add_native_fn("any", 1, |vm_ref, args| {
            let target = &args[0];
            match target {
                Value::Array(a) => Value::Bool(a.borrow().iter().any(|x| x.is_truthy())),
                _ => {
                    vm_ref.runtime_error("Expected array for any");
                    Value::Null
                }
            }
        });
        self.add_native_fn("all", 1, |vm_ref, args| {
            let target = &args[0];
            match target {
                Value::Array(a) => Value::Bool(a.borrow().iter().all(|x| x.is_truthy())),
                _ => {
                    vm_ref.runtime_error("Expected array for any");
                    Value::Null
                }
            }
        });
        self.add_native_fn("even", 1, |vm_ref, args| {
            let target = &args[0];
            match target {
                Value::Number(n) => Value::Bool(n % 2.0 == 0.0),
                _ => {
                    vm_ref.runtime_error("Expected array for any");
                    Value::Null
                }
            }
        });
        self.add_native_fn("max", 1, |vm_ref, args| {
            Self::fold_numbers(vm_ref, &args[0], -f64::INFINITY, f64::max, "max")
        });
        self.add_native_fn("min", 1, |vm_ref, args| {
            Self::fold_numbers(vm_ref, &args[0], f64::INFINITY, f64::min, "min")
        });
        self.add_native_fn("sum", 1, |vm_ref, args| {
            Self::fold_numbers(vm_ref, &args[0], 0.0, |a, b| a + b, "sum")
        });
        self.add_native_fn("int", 1, |vm_ref, args| {
            let target = &args[0];
            match target {
                Value::Number(n) => Value::Number(n.floor()),
                Value::String(s) => {
                    let s_str: String = s.borrow().iter().collect();
                    match s_str.parse::<f64>() {
                        Ok(n) => Value::Number(n.floor()),
                        Err(_) => {
                            vm_ref
                                .runtime_error(&format!("Expected number for int, got {}", s_str));
                            Value::Null
                        }
                    }
                }
                _ => {
                    vm_ref.runtime_error(&format!("Expected number for int, got {:?}", target));
                    Value::Null
                }
            }
        });
        self.add_native_fn("abs", 1, |vm_ref, args| {
            let target = &args[0];
            match target {
                Value::Number(n) => Value::Number(n.abs()),
                _ => {
                    vm_ref.runtime_error("Expected number for abs");
                    Value::Null
                }
            }
        });
    }

    pub fn new(chunk: Chunk) -> Self {
        let mut result = Self {
            stack: Vec::new(),
            ip: 0,
            chunk,
            internally_running_fns: 0,
            had_runtime_error: false,
            globals: HashMap::new(),
            frame_starts: vec![0],
        };
        result.add_native_fns();
        result
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

    fn semi_local_idx_on_stack(&mut self) -> usize {
        let local_idx = self.read_byte() as usize;
        let frame_starts_distance = self.read_byte() as usize;
        self.frame_starts[self.frame_starts.len() - 1 - frame_starts_distance] + local_idx
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

    pub fn runtime_error(&mut self, message: &str) {
        eprint!("{}", message);
        eprintln!(" at [line {}] in script", self.chunk.lines[self.ip - 1]);
        self.had_runtime_error = true;
    }

    fn check_arity(&mut self, expected: u8, got: usize) -> bool {
        if expected as usize != got {
            self.runtime_error(&format!("Expected {} arguments but got {}", expected, got));
            return false;
        }
        true
    }

    fn initialize_cigg_function(&mut self, arg_c: usize) {
        let function = match self.peek(arg_c) {
            Value::Function(f) => f,
            other => panic!("Expected function , got {}", other),
        };
        let start = function.start;
        if !self.check_arity(function.arity, arg_c) {
            return;
        }
        let return_address = Value::ReturnAddress(self.ip);
        self.ip = start;
        // TODO PERF: don't really like the idea of inserting with an offset
        self.insert(return_address, self.stack.len() - arg_c - 1);
        self.frame_starts.push(self.stack.len() - arg_c - 1);
    }

    fn execute_cigg_function(&mut self, arg_c: usize) -> Value {
        self.initialize_cigg_function(arg_c);
        self.internally_running_fns += 1;
        self.run();
        self.internally_running_fns -= 1;
        self.pop()
    }

    fn call_native_function(&mut self, arg_c: usize) {
        let args = self.stack.split_off(self.stack.len() - arg_c);
        let native_fn = match self.peek(0) {
            Value::NativeFn(nf) => nf,
            _ => unreachable!(),
        };
        let function = native_fn.function;
        if !self.check_arity(native_fn.arity, arg_c) {
            return;
        }
        let result = function(self, &args);
        // TODO: Not really sure why this pop has to be here? But it does...
        self.pop();
        self.push(result);
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
                    self.push(Value::String(Rc::new(RefCell::new(a.chars().collect()))));
                    return;
                }
                if a.is_array() && b.is_array() {
                    let a = match a {
                        Value::Array(a) => a,
                        _ => unreachable!(),
                    };
                    let b = match b {
                        Value::Array(b) => b,
                        _ => unreachable!(),
                    };
                    let mut result = Vec::new();
                    result.extend(a.borrow().iter().cloned());
                    result.extend(b.borrow().iter().cloned());
                    self.push(Value::Array(Rc::new(RefCell::new(result))));
                    return;
                }
                if !a.is_number() || !b.is_number() {
                    self.runtime_error(&format!(
                        "Expected numbers, a string, or two arrays for add operation, but got: {:?} and {:?}",
                        a, b
                    ));
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
                    self.push(Value::String(Rc::new(RefCell::new(
                        a.chars().cycle().take(a.len() * b as usize).collect(),
                    ))));
                    return;
                }
                if a.is_array() && b.is_number() {
                    let a = match a {
                        Value::Array(a) => a,
                        _ => unreachable!(),
                    };
                    let b = b.as_number();
                    let mut result = Vec::new();
                    for _ in 0..(b as usize) {
                        result.extend(a.borrow().iter().cloned());
                    }
                    self.push(Value::Array(Rc::new(RefCell::new(result))));
                    return;
                }
                if !a.is_number() || !b.is_number() {
                    self.runtime_error("Bad operators to mul (*)");
                    return;
                }
                self.push(Value::Number(a.as_number() * b.as_number()));
            }
            OpCode::Div | OpCode::DivInt => {
                if a.is_string() || b.is_string() {
                    self.runtime_error("Cannot divide strings");
                    return;
                }
                let result = a.as_number() / b.as_number();
                if op == OpCode::DivInt {
                    self.push(Value::Number(result.floor()));
                    return;
                }
                self.push(Value::Number(result));
            }
            OpCode::Equal => self.push(Value::Bool(a == b)),
            OpCode::Mod => {
                if !a.is_number() || !b.is_number() {
                    self.runtime_error("Expected numbers to mod operation");
                    return;
                }
                self.push(Value::Number(a.as_number() % b.as_number()));
            }
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

    fn global_identifier(&mut self) -> String {
        let name_index = self.read_byte();
        let name = self.chunk.constants[name_index as usize].clone();
        if !name.is_string() {
            self.runtime_error("Expected string for global name");
        }
        name.as_string()
    }

    fn index_access(&mut self, target: &Value, index: Value) {
        let result = match target {
            Value::Array(a) => a.borrow()[index.as_number() as usize].clone(),
            Value::String(s) => {
                let s = s.borrow();
                Value::String(Rc::new(RefCell::new(vec![s[index.as_number() as usize]])))
            }
            Value::HashMap(m) => {
                let m = m.borrow();
                m.0.get(&index).unwrap().clone()
            }
            _ => unreachable!(),
        };
        self.push(result);
    }

    fn collect_from_range<T: Clone>(&self, slice: &[T], range: &mut Range) -> Vec<T> {
        let mut result = Vec::new();
        while let Some(i) = range.next() {
            let idx = i.as_number() as usize;
            result.push(slice[idx].clone());
        }
        result
    }

    fn range_access(&mut self, target: &Value, range: &mut Range) {
        let result = match target {
            Value::Array(a) => {
                let a = a.borrow();
                let collected = self.collect_from_range(&a, range);
                Value::Array(Rc::new(RefCell::new(collected)))
            }
            Value::String(s) => {
                let s = s.borrow();
                let collected = self.collect_from_range(&s, range);
                Value::String(Rc::new(RefCell::new(collected)))
            }
            _ => unreachable!(),
        };
        self.push(result);
    }

    fn access_ops(&mut self, op: OpCode) {
        let mut value_to_set = Value::Null;
        if op == OpCode::AccessSet {
            value_to_set = self.pop();
        }
        let index = self.pop();
        // If it's a set, the whole thing will be an expression statement
        // and pop at the end. So leave the reference on the stack to
        // account for that one.
        let reference: Value = if op == OpCode::AccessSet {
            self.peek(0).clone()
        } else {
            self.pop()
        };
        let target = match reference {
            Value::Array(_) | Value::String(_) | Value::HashMap(_) => reference,
            _ => {
                self.runtime_error("Attempt to index non-indexable value");
                return;
            }
        };
        if let Some(error) = target.check_valid_access(&index, op == OpCode::AccessSet) {
            self.runtime_error(&error);
            return;
        }
        match op {
            OpCode::Access => {
                match index {
                    Value::Range(mut r) => self.range_access(&target, &mut r),
                    // Default case since the value might be e.g. a string accessing a hm
                    _ => {
                        self.index_access(&target, index.clone());
                    }
                }
            }
            OpCode::AccessSet => match target {
                Value::Array(a) => a.borrow_mut()[index.as_number() as usize] = value_to_set,
                Value::String(_) => {
                    todo!();
                }
                Value::HashMap(m) => {
                    let mut m = m.borrow_mut();
                    m.0.insert(index, value_to_set);
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn to_iterable_vec(&self, val: Value) -> Option<Vec<Value>> {
        match val {
            Value::Array(a) => Some(a.borrow().clone()),
            // TODO PERF: String iteration might be slow.
            Value::String(s) => {
                let chars = s.borrow();
                Some(
                    chars
                        .iter()
                        .map(|ch| Value::String(Rc::new(RefCell::new(vec![*ch]))))
                        .collect(),
                )
            }
            Value::HashMap(hm) => {
                let mut keys: Vec<Value> = hm.borrow().0.keys().cloned().collect();
                keys.sort();
                Some(keys)
            }
            Value::Range(r) => {
                let mut range = r.clone();
                let mut vec = Vec::new();
                while let Some(n) = range.next() {
                    vec.push(n);
                }
                Some(vec)
            }
            _ => panic!("Expected iterable value, got {:?}", val),
        }
    }

    fn get_and_push_stack_var(&mut self, idx: usize) {
        let value = self.stack[idx].clone();
        self.push(value);
    }

    fn set_stack_var(&mut self, idx: usize) {
        let value = self.peek(0).clone();
        self.stack[idx] = value;
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
                | OpCode::DivInt
                | OpCode::Equal
                | OpCode::Mod
                | OpCode::NotEqual
                | OpCode::Greater
                | OpCode::GreaterEqual => {
                    self.binary_op(instruction);
                }
                OpCode::Access | OpCode::AccessSet => {
                    self.access_ops(instruction);
                }
                OpCode::Array => {
                    let count = self.read_byte();
                    let mut array = Vec::new();
                    for _ in 0..count {
                        let item = self.pop();
                        if item.is_range() {
                            let mut item = match item {
                                Value::Range(r) => r,
                                _ => unreachable!(),
                            };
                            let mut temp = Vec::new();
                            while let Some(next) = item.next() {
                                temp.push(next);
                            }
                            temp.reverse();
                            array.extend(temp);
                            continue;
                        }
                        array.push(item);
                    }
                    array.reverse();
                    self.push(Value::Array(Rc::new(RefCell::new(array))));
                }
                // I'm not sure Values work perfectly as keys the PartialEq/Eq implementations is
                // dodgy
                OpCode::HashMap => {
                    let count = self.read_byte();
                    let mut map = HashMap::new();
                    for _ in 0..count {
                        let value = self.pop();
                        let key = self.pop();
                        map.insert(key, value);
                    }
                    self.push(Value::HashMap(Rc::new(RefCell::new(CiggHashMap(map)))));
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
                    let result = Value::Bool(!&value.is_truthy());
                    self.push(result);
                }
                OpCode::Next => {
                    let iter_val = self.pop();
                    let mut iter: Box<dyn Iterable> = match iter_val {
                        Value::Iterable(iter_box) => iter_box,
                        _ => {
                            if let Some(vec) = self.to_iterable_vec(iter_val) {
                                Box::new(Iterator {
                                    vec: Rc::new(RefCell::new(vec)),
                                    current: 0,
                                })
                            } else {
                                self.runtime_error("Expected an iterable");
                                return VmResult::RuntimeError;
                            }
                        }
                    };
                    match iter.next() {
                        Some(n) => {
                            self.push(Value::Iterable(iter));
                            self.push(n);
                        }
                        None => {
                            self.push(Value::Null);
                        }
                    }
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::Extend => {
                    let to_append = self.pop();
                    let target = self.pop();
                    match target {
                        Value::Array(a) => a.borrow_mut().push(to_append),
                        Value::String(a) => a.borrow_mut().extend(to_append.as_string().chars()),
                        _ => {
                            self.runtime_error("Expected array or string");
                            return VmResult::RuntimeError;
                        }
                    }
                    self.push(Value::Null);
                }
                OpCode::Duplicate => {
                    let value = self.peek(0).clone();
                    self.push(value);
                }
                OpCode::Dup2 => {
                    let first = self.peek(1).clone();
                    let second = self.peek(0).clone();
                    self.push(first);
                    self.push(second);
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
                    self.get_and_push_stack_var(idx);
                }
                OpCode::SetLocal => {
                    let idx = self.local_idx_on_stack();
                    self.set_stack_var(idx);
                }
                OpCode::GetSemiLocal => {
                    let idx = self.semi_local_idx_on_stack();
                    self.get_and_push_stack_var(idx);
                }
                OpCode::SetSemiLocal => {
                    let idx = self.semi_local_idx_on_stack();
                    self.set_stack_var(idx);
                }
                OpCode::In => {
                    let target = self.pop();
                    let key = self.pop();
                    let result = match target {
                        Value::Array(a) => {
                            let a = a.borrow();
                            a.iter().any(|x| x == &key)
                        }
                        Value::String(_) => {
                            todo!();
                        }
                        Value::HashMap(m) => {
                            let m = m.borrow();
                            m.0.contains_key(&key)
                        }
                        _ => {
                            self.runtime_error("Expected array, string or hashmap to in op");
                            return VmResult::RuntimeError;
                        }
                    };
                    self.push(Value::Bool(result));
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short() as usize;
                    if !self.peek(0).is_truthy() {
                        self.ip += offset;
                    }
                }
                OpCode::JumpIfTrue => {
                    let offset = self.read_short() as usize;
                    if self.peek(0).is_truthy() {
                        self.ip += offset;
                    }
                }
                OpCode::JumpIfNull => {
                    let offset = self.read_short() as usize;
                    if let Value::Null = self.peek(0) {
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
                    match called {
                        Value::Function(_) => self.initialize_cigg_function(arg_c),
                        Value::NativeFn(_) => self.call_native_function(arg_c),
                        _ => {
                            self.runtime_error("Can only call functions");
                            return VmResult::RuntimeError;
                        }
                    };
                }
                OpCode::Range => {
                    let step = self.pop();
                    let end = self.pop().as_number() as i32;
                    let start = self.pop().as_number() as i32;
                    let range = Value::Range(Range::new(start, end, step));
                    self.push(range);
                }
                OpCode::Null => self.push(Value::Null),
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Transform => {
                    let transform = self.read_byte();
                    (TRANSFORMATION_FNS[transform as usize].function)(self);
                }
                OpCode::RaiseError => {
                    let message = self.pop();
                    self.runtime_error(&format!("Error: {}", message));
                    return VmResult::RuntimeError;
                }
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
                    if self.internally_running_fns > 0 {
                        return VmResult::OK;
                    }
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
        let program = "global a = 1; print(a); a = 5; print(a);";
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
        let program = "print(b);";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::RuntimeError);
    }

    #[test]
    fn block_scoped() {
        let program = "let a = 1; { let a = 5; print(a); }";
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
        let program = "fn f(a) { print(a); } f(5);";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::OK);
        let program = "fn f(a) { print(a); } f(5,10);";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::RuntimeError);
    }

    #[test]
    fn basic_hm() {
        let program = "let hm = {1: 2, 3: 4}; print(hm[1]);";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::OK);
        let program = "let hm = {1: 2, 3: 4}; print(hm[10]);";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::RuntimeError);
    }

    #[test]
    fn bad_add() {
        let program = "let x = {'cats': 5} + 5;";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::RuntimeError);
        let program = "[1,2,3] + 5;";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::RuntimeError);
    }

    #[test]
    fn simple_sort() {
        let program = "let xs = [5,2,3,4,1]; print(sort xs); print(xs);";
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::OK);
    }

    #[test]
    fn no_corrupting_state_when_nested_fns() {
        let program = r#"
let arrs = [[1]];
fn safe(xs) {
  return map ((x) => true) xs;
}
print(filter safe arrs);"#;
        let mut vm = get_vm(program);
        let result = vm.run();
        assert_eq!(result, VmResult::OK);
    }
}
