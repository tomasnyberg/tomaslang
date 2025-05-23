use std::any::Any;
use std::fmt::Debug;
use std::ops::Shr;
use std::rc::Rc;
use std::{cell::RefCell, collections::HashMap};

use crate::{chunk::*, scanner::*, vm::Value, vm::TRANSFORMATION_FNS};

macro_rules! active_chunk {
    ($this:expr) => {
        &mut $this.compiling_funcs.last_mut().unwrap().chunk
    };
}

struct Compiler {
    tokens: Vec<Token>,
    current: usize,
    error: bool,
    compiling_funcs: Vec<Function>,
    compiled_funcs: Vec<Function>,
    rules: HashMap<TokenType, ParseRule>,
    scope_depth: usize,
    // For continue: keep track of (loop_start, scope_depth)
    loop_continue_tracker: Vec<(usize, usize)>,
    // For break: keep track of jumps that want to jump to the end
    loop_endjumps: Vec<Vec<usize>>,
}

#[derive(Debug, Clone, PartialEq)]
struct Local {
    name: Token,
    depth: i32,
    constant: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub start: usize,
    pub name: Token,
    pub chunk: Chunk,
    locals: Vec<Local>,
    pub arity: u8,
    const_idx: usize,
}

pub trait Iterable: Debug {
    fn next(&mut self) -> Option<Value>;
    fn clone_box(&self) -> Box<dyn Iterable>;
    fn eq_box(&self, other: &dyn Iterable) -> bool;
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub start: i32,
    pub end: i32,
    pub current: i32,
    pub step: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Iterator {
    pub vec: Rc<RefCell<Vec<Value>>>,
    pub current: usize,
}

impl Range {
    pub fn new(start: i32, end: i32, step: Value) -> Self {
        let step = if let Value::Number(_) = step {
            step.as_number() as i32
        } else if start < end {
            1
        } else {
            -1
        };
        Self {
            start,
            end,
            current: start,
            step,
        }
    }

    pub fn as_debug_string(&self) -> String {
        let lower = if self.current > self.start {
            self.current
        } else {
            self.start
        };
        let upper = if self.current < self.end {
            self.end
        } else {
            self.current
        };
        format!("R {}..{}:{}", lower, upper, self.step)
    }
}

impl Iterable for Range {
    fn next(&mut self) -> Option<Value> {
        if (self.end - self.current) * self.step <= 0 {
            return None;
        }
        let current = self.current;
        self.current += self.step;
        Some(Value::Number(current as f64))
    }
    fn clone_box(&self) -> Box<dyn Iterable> {
        Box::new(self.clone())
    }
    fn eq_box(&self, other: &dyn Iterable) -> bool {
        if let Some(other_range) = other.as_any().downcast_ref::<Range>() {
            self == other_range
        } else {
            false
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Iterable for Iterator {
    fn next(&mut self) -> Option<Value> {
        if self.current == self.vec.borrow().len() {
            None
        } else {
            let value = self.vec.borrow()[self.current].clone();
            self.current += 1;
            Some(value)
        }
    }
    fn clone_box(&self) -> Box<dyn Iterable> {
        Box::new(self.clone())
    }
    fn eq_box(&self, other: &dyn Iterable) -> bool {
        if let Some(other_iter) = other.as_any().downcast_ref::<Iterator>() {
            self == other_iter
        } else {
            false
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Clone for Box<dyn Iterable> {
    fn clone(&self) -> Box<dyn Iterable> {
        self.clone_box()
    }
}

impl PartialEq for Box<dyn Iterable> {
    fn eq(&self, other: &Self) -> bool {
        self.eq_box(&**other)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    Access,
    AccessSet,
    Array,
    Add,
    Sub,
    Mul,
    Div,
    DivInt,
    Duplicate,
    Dup2,
    Extend,
    Equal,
    Mod,
    NotEqual,
    Greater,
    GreaterEqual,
    HashMap,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    GetSemiLocal,
    SetSemiLocal,
    In,
    JumpIfFalse,
    JumpIfTrue,
    JumpIfNull,
    Jump,
    Loop,
    Call,
    Negate,
    Not,
    Next,
    Pop,
    Range,
    RaiseError,
    Null,
    True,
    False,
    Transform,
    Return,
    Eof,
}

#[derive(Copy, Clone, PartialOrd, PartialEq)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn next(&self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CompilerResult {
    Chunk(Chunk),
    CompileError,
}

type ParseFn = fn(&mut Compiler);

#[derive(Clone, Copy)]
struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl OpCode {
    pub fn from_u8(byte: u8) -> Self {
        match byte {
            0 => OpCode::Constant,
            1 => OpCode::Access,
            2 => OpCode::AccessSet,
            3 => OpCode::Array,
            4 => OpCode::Add,
            5 => OpCode::Sub,
            6 => OpCode::Mul,
            7 => OpCode::Div,
            8 => OpCode::DivInt,
            9 => OpCode::Duplicate,
            10 => OpCode::Dup2,
            11 => OpCode::Extend,
            12 => OpCode::Equal,
            13 => OpCode::Mod,
            14 => OpCode::NotEqual,
            15 => OpCode::Greater,
            16 => OpCode::GreaterEqual,
            17 => OpCode::HashMap,
            18 => OpCode::DefineGlobal,
            19 => OpCode::GetGlobal,
            20 => OpCode::SetGlobal,
            21 => OpCode::GetLocal,
            22 => OpCode::SetLocal,
            23 => OpCode::GetSemiLocal,
            24 => OpCode::SetSemiLocal,
            25 => OpCode::In,
            26 => OpCode::JumpIfFalse,
            27 => OpCode::JumpIfTrue,
            28 => OpCode::JumpIfNull,
            29 => OpCode::Jump,
            30 => OpCode::Loop,
            31 => OpCode::Call,
            32 => OpCode::Negate,
            33 => OpCode::Not,
            34 => OpCode::Next,
            35 => OpCode::Pop,
            36 => OpCode::Range,
            37 => OpCode::RaiseError,
            38 => OpCode::Null,
            39 => OpCode::True,
            40 => OpCode::False,
            41 => OpCode::Transform,
            42 => OpCode::Return,
            43 => OpCode::Eof,
            _ => panic!("unexpected opcode (did you update this match after adding an op?)"),
        }
    }
}

impl Compiler {
    #[rustfmt::skip]
    fn create_rules() -> HashMap<TokenType, ParseRule> {
        let mut rules: HashMap<TokenType, ParseRule> = HashMap::new();

        let mut rule = |token, prefix, infix, precedence| {
            rules.insert(
                token,
                ParseRule {
                    prefix,
                    infix,
                    precedence,
                },
            );
        };
        use TokenType::*;
        rule(LeftParen,     Some(Self::grouping),      Some(Self::call),   Precedence::Call);
        rule(RightParen,    None,                      None,               Precedence::None);
        rule(LeftBrace,     Some(Self::hash_map),      None,               Precedence::None);
        rule(RightBrace,    None,                      None,               Precedence::None);
        rule(LeftBracket,   Some(Self::array),         Some(Self::access), Precedence::Call);
        rule(RightBracket,  None,                      None,               Precedence::None);
        rule(BigRightArrow, None,                      None,               Precedence::None);
        rule(Comma,         None,                      None,               Precedence::None);
        rule(QuestionMark,  None,                      Some(Self::ternary),Precedence::Assignment);
        rule(Colon,         None,                      None,               Precedence::None);
        rule(ColonColon,    None,                      Some(Self::append), Precedence::Term);
        rule(Dot,           None,                      None,               Precedence::None);
        // Is precedence correct here? Not sure.
        rule(DotDot,        None,                      Some(Self::range),  Precedence::Term);
        rule(Minus,         Some(Self::unary),         Some(Self::binary), Precedence::Term);
        rule(Plus,          None,                      Some(Self::binary), Precedence::Term);
        rule(Semicolon,     None,                      None,               Precedence::None);
        rule(Slash,         None,                      Some(Self::binary), Precedence::Factor);
        rule(SlashDown,     None,                      Some(Self::binary), Precedence::Factor);
        rule(Star,          None,                      Some(Self::binary), Precedence::Factor);
        rule(Bang,          Some(Self::unary),         None,               Precedence::None);
        rule(BangEqual,     None,                      Some(Self::binary), Precedence::Equality);
        rule(Equal,         None,                      None,               Precedence::None);
        rule(EqualEqual,    None,                      Some(Self::binary), Precedence::Equality);
        rule(Greater,       None,                      Some(Self::binary), Precedence::Comparison);
        rule(GreaterEqual,  None,                      Some(Self::binary), Precedence::Comparison);
        rule(Less,          None,                      Some(Self::binary), Precedence::Comparison);
        rule(LessEqual,     None,                      Some(Self::binary), Precedence::Comparison);
        rule(Percent,       None,                      Some(Self::binary), Precedence::Factor);
        rule(Identifier,    Some(Self::variable),      None,               Precedence::None);
        rule(String,        Some(Self::string),        None,               Precedence::None);
        rule(Number,        Some(Self::number),        None,               Precedence::None);
        rule(And,           None,                      Some(Self::and),    Precedence::And);
        rule(Match,         Some(Self::match_expr),    None,               Precedence::None);
        rule(Else,          None,                      None,               Precedence::None);
        rule(False,         Some(Self::literal),       None,               Precedence::None);
        rule(Fn,            None,                      None,               Precedence::None);
        rule(For,           None,                      None,               Precedence::None);
        rule(If,            None,                      None,               Precedence::None);
        rule(In,            None,                      Some(Self::in_op),  Precedence::Comparison);
        rule(Null,          Some(Self::literal),       None,               Precedence::None);
        rule(Or,            None,                      Some(Self::or),     Precedence::Or);
        rule(Return,        None,                      None,               Precedence::None);
        rule(Super,         None,                      None,               Precedence::None);
        rule(This,          None,                      None,               Precedence::None);
        rule(True,          Some(Self::literal),       None,               Precedence::None);
        rule(Transformation,Some(Self::transformation),None,               Precedence::Call);
        rule(Let,           None,                      None,               Precedence::None);
        rule(While,         None,                      None,               Precedence::None);
        rule(Error,         None,                      None,               Precedence::None);
        rule(Eof,           None,                      None,               Precedence::None);

        rules
    }

    pub fn new(input: &str) -> Self {
        let tokens = scan(input);
        let mainfn = Function {
            start: 0,
            name: Token::new(TokenType::Eof, "main".to_string(), 0),
            chunk: Chunk::new(),
            locals: Vec::new(),
            arity: 0,
            const_idx: 0,
        };

        Self {
            tokens,
            current: 0,
            error: false,
            compiling_funcs: vec![mainfn],
            compiled_funcs: Vec::new(),
            rules: Self::create_rules(),
            scope_depth: 0,
            loop_continue_tracker: Vec::new(),
            loop_endjumps: Vec::new(),
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) {
        if self.peek(0).token_type == token_type {
            self.advance();
            return;
        }
        self.error_at_current(message);
    }

    fn peek(&self, offset: i32) -> &Token {
        assert!((self.current as i32 - offset) >= 0);
        &self.tokens[(self.current as i32 - offset) as usize]
    }

    fn check(&mut self, token_type: TokenType) -> bool {
        self.peek(0).token_type == token_type
    }

    fn error_at_current(&mut self, message: &str) {
        if self.error {
            return;
        }
        self.error = true;
        let token = &self.peek(0);
        eprintln!(
            "[line {}] Error at '{}': {}",
            token.line, token.lexeme, message
        );
    }

    fn emit_exception(&mut self, message: &str) {
        let string = Value::String(Rc::new(RefCell::new(message.chars().collect())));
        self.emit_constant(string);
        self.emit_byte(OpCode::RaiseError as u8, true);
    }

    fn match_(&mut self, token_type: TokenType) -> bool {
        if self.peek(0).token_type == token_type {
            self.advance();
            return true;
        }
        false
    }

    fn argument_list(&mut self) -> u8 {
        let mut arg_c = 0;
        while !self.check(TokenType::RightParen) {
            self.expression();
            arg_c += 1;
            if !self.match_(TokenType::Comma) {
                break;
            }
        }
        arg_c
    }

    fn call(&mut self) {
        let arg_c = self.argument_list();
        self.emit_constant(Value::Number(arg_c as f64));
        self.consume(TokenType::RightParen, "Expected ')' after arguments");
        self.emit_byte(OpCode::Call as u8, true);
    }

    fn emit_transform(&mut self, i: usize) {
        self.emit_byte(OpCode::Transform as u8, true);
        self.emit_byte(i as u8, true);
    }

    fn transformation(&mut self) {
        for (i, tf) in TRANSFORMATION_FNS.iter().enumerate() {
            if self.peek(1).lexeme == tf.name {
                match tf.name {
                    "map" | "filter" | "takeWhile" => {
                        if self.peek(0).token_type == TokenType::LeftParen {
                            self.consume(TokenType::LeftParen, "Expected '(' after transformation");
                            self.expression();
                            self.consume(
                                TokenType::RightParen,
                                "Expected ')' after transformation",
                            );
                        } else if self.peek(0).token_type == TokenType::Identifier {
                            // variable() expects the identifier to have been consumed
                            self.advance();
                            self.variable();
                        } else {
                            self.error_at_current(
                                "Expected '(' or identifier after transformation",
                            );
                        }
                        self.expression();
                        self.emit_transform(i);
                    }
                    "words" => {
                        let first_arg_type = self.peek(0).token_type;
                        let second_arg_type = self.peek(-1).token_type;
                        // Do we have 'words " " s' or 'words delim s'?
                        if second_arg_type == TokenType::Identifier
                            || second_arg_type == TokenType::String
                        {
                            self.advance();
                            if first_arg_type == TokenType::String {
                                self.string();
                            } else if first_arg_type == TokenType::Identifier {
                                self.variable();
                            } else {
                                self.error_at_current(
                                    "Expected string or identifier as delimiter to words",
                                );
                            }
                            self.expression();
                            self.emit_transform(i);
                        } else {
                            self.expression();
                            self.emit_transform(i + 1); // Output words_simple instead.
                        }
                    }
                    "sort" => {
                        self.expression();
                        self.emit_transform(i);
                    }
                    _ => {}
                }

                return;
            }
        }
    }

    fn literal(&mut self) {
        match self.peek(1).token_type {
            TokenType::Null => self.emit_byte(OpCode::Null as u8, true),
            TokenType::True => self.emit_byte(OpCode::True as u8, true),
            TokenType::False => self.emit_byte(OpCode::False as u8, true),
            _ => self.error_at_current("Expected literal (Null, True, False)"),
        }
    }

    // Looks through all the compiling functions to find also ones that are in enclosing scopes.
    fn resolve_local(&mut self, name: &Token) -> Option<(u8, u8)> {
        for (distance, func) in self.compiling_funcs.iter().rev().enumerate() {
            for (i, local) in func.locals.iter().enumerate().rev() {
                if name.lexeme == local.name.lexeme {
                    if local.depth == -1 {
                        self.error_at_current("Cannot read local variable in its own initializer");
                    }
                    return Some((distance as u8, i as u8));
                }
            }
        }
        None
    }

    fn ensure_not_const(&mut self, resolved: Option<(u8, u8)>) {
        if let Some((distance, index)) = resolved {
            let func_index = self.compiling_funcs.len() - 1 - (distance as usize);
            if self.compiling_funcs[func_index].locals[index as usize].constant {
                self.error_at_current("Cannot assign to constant variable");
            }
        }
    }

    // TODO: Disallow assigning in weird ways e.g a + (b = 1)
    fn variable(&mut self) {
        let var_token = self.peek(1).clone();
        let set_op: OpCode;
        let get_op: OpCode;
        let arg: u8;
        let resolved = self.resolve_local(&var_token);
        if let Some((distance, index)) = resolved {
            arg = index;
            if distance > 0 {
                set_op = OpCode::SetSemiLocal;
                get_op = OpCode::GetSemiLocal;
            } else {
                set_op = OpCode::SetLocal;
                get_op = OpCode::GetLocal;
            }
        } else {
            arg = self.identifier_constant();
            set_op = OpCode::SetGlobal;
            get_op = OpCode::GetGlobal;
        }
        let operator = self.peek(0).token_type;
        match operator {
            TokenType::Equal => {
                self.advance();
                self.ensure_not_const(resolved);
                self.expression();
                self.emit_bytes(set_op as u8, arg);
            }
            TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::StarEqual
            | TokenType::SlashEqual
            | TokenType::SlashDownEqual
            | TokenType::PercentEqual => {
                self.advance();
                self.ensure_not_const(resolved);
                self.emit_bytes(get_op as u8, arg);
                self.expression();
                self.emit_compound_operator(operator);
                self.emit_bytes(set_op as u8, arg);
            }
            _ => self.emit_bytes(get_op as u8, arg),
        }
        if let Some((distance, _)) = resolved {
            if distance > 0 {
                self.emit_byte(distance, false);
            }
        }
    }

    fn unary(&mut self) {
        let operator = self.peek(1).token_type;
        self.parse_precedence(Precedence::Unary);
        match operator {
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8, true),
            TokenType::Bang => self.emit_byte(OpCode::Not as u8, true),
            _ => self.error_at_current("Expected unary operator"),
        }
    }

    fn ternary(&mut self) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop as u8, true);
        self.expression();
        let end_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(else_jump);
        self.emit_byte(OpCode::Pop as u8, true);
        self.consume(TokenType::Colon, "Expected ':' after ternary condition");
        self.expression();
        self.patch_jump(end_jump);
    }

    fn emit_jump(&mut self, jump_type: OpCode) -> usize {
        assert!([
            OpCode::Jump,
            OpCode::JumpIfFalse,
            OpCode::JumpIfTrue,
            OpCode::JumpIfNull
        ]
        .contains(&jump_type));
        self.emit_byte(jump_type as u8, true);
        self.emit_byte(0xff, false);
        self.emit_byte(0xff, false);
        active_chunk!(self).code.len() - 2
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(OpCode::Loop as u8, true);
        let chunk = active_chunk!(self);
        let offset = chunk.code.len() - loop_start + 2;
        if offset > 0xffff {
            self.error_at_current("Loop body too large");
        }
        self.emit_byte(offset.shr(8) as u8, false);
        self.emit_byte(offset as u8, false);
    }

    fn patch_jump(&mut self, offset: usize) {
        if offset > 0xffff {
            self.error_at_current("Too much code to jump over");
        }
        let chunk = active_chunk!(self);
        let jump = chunk.code.len() - offset - 2;
        chunk.code[offset] = ((jump >> 8) & 0xff) as u8;
        chunk.code[offset + 1] = (jump & 0xff) as u8;
        chunk.debug_code[offset] = format!("{:3} (op)", chunk.code[offset]);
        chunk.debug_code[offset + 1] = format!("{:3} (op)", chunk.code[offset + 1]);
    }

    fn emit_constant(&mut self, value: Value) -> u8 {
        let index = self.make_constant(value);
        self.emit_bytes(OpCode::Constant as u8, index);
        index
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let first_chunk = &mut self.compiling_funcs[0].chunk;
        let index: u8 = first_chunk.constants.len() as u8;
        first_chunk.constants.push(value);
        if first_chunk.constants.len() > 256 {
            self.error_at_current("Too many constants in one chunk");
        }
        index
    }

    fn string(&mut self) {
        if self.peek(1).token_type == TokenType::String {
            let string = self.peek(1).lexeme.clone();
            let string: Vec<char> = string.chars().collect();
            let string: Value = Value::String(Rc::new(RefCell::new(string)));
            self.emit_constant(string);
            return;
        }
        self.error_at_current("Expected string");
    }

    fn number(&mut self) {
        if self.peek(1).token_type == TokenType::Number {
            let number = self.peek(1).lexeme.parse::<f64>().unwrap();
            self.emit_constant(Value::Number(number));
            return;
        }
        self.error_at_current("Expected number");
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn emit_byte(&mut self, byte: u8, is_op: bool) {
        // Push code to the functions chunk rather than the script
        let line = self.peek(1).line as usize;
        let chunk = active_chunk!(self);
        chunk.code.push(byte);
        chunk.lines.push(line);
        let to_push = if is_op {
            format!("{:3} {:?}", byte, OpCode::from_u8(byte))
        } else {
            format!("{:3} (op)", byte)
        };
        chunk.debug_code.push(to_push);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1, true);
        self.emit_byte(byte2, false);
    }

    fn and(&mut self) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop as u8, true);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn or(&mut self) {
        let end_jump = self.emit_jump(OpCode::JumpIfTrue);
        self.emit_byte(OpCode::Pop as u8, true);
        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn range(&mut self) {
        self.expression();
        if self.match_(TokenType::Colon) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Null as u8, true);
        }
        self.emit_byte(OpCode::Range as u8, true);
    }

    fn emit_compound_operator(&mut self, operator: TokenType) {
        match operator {
            TokenType::PlusEqual => self.emit_byte(OpCode::Add as u8, true),
            TokenType::MinusEqual => self.emit_byte(OpCode::Sub as u8, true),
            TokenType::StarEqual => self.emit_byte(OpCode::Mul as u8, true),
            TokenType::SlashEqual => self.emit_byte(OpCode::Div as u8, true),
            TokenType::SlashDownEqual => self.emit_byte(OpCode::DivInt as u8, true),
            TokenType::PercentEqual => self.emit_byte(OpCode::Mod as u8, true),
            _ => self.error_at_current("Expected compound assignment operator"),
        }
    }

    fn emit_operator(&mut self, operator: TokenType) {
        match operator {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8, true),
            TokenType::Minus => self.emit_byte(OpCode::Sub as u8, true),
            TokenType::Star => self.emit_byte(OpCode::Mul as u8, true),
            TokenType::Slash => self.emit_byte(OpCode::Div as u8, true),
            TokenType::SlashDown => self.emit_byte(OpCode::DivInt as u8, true),
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal as u8, true),
            TokenType::BangEqual => self.emit_byte(OpCode::NotEqual as u8, true),
            TokenType::Greater => self.emit_byte(OpCode::Greater as u8, true),
            TokenType::GreaterEqual => self.emit_byte(OpCode::GreaterEqual as u8, true),
            TokenType::Less => self.emit_bytes(OpCode::GreaterEqual as u8, OpCode::Not as u8),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            TokenType::Percent => self.emit_byte(OpCode::Mod as u8, true),
            _ => self.error_at_current("Expected binary operator"),
        }
    }

    fn binary(&mut self) {
        let operator = self.peek(1).token_type;
        let rule: ParseRule = self.rules[&operator];
        self.parse_precedence(rule.precedence.next());
        self.emit_operator(operator);
    }

    fn append(&mut self) {
        self.expression();
        self.emit_byte(OpCode::Extend as u8, true);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::If, "Expected 'if' to start if statement");
        self.expression();
        // Jump over the then block
        let over_then_jump = self.emit_jump(OpCode::JumpIfFalse);
        // Pop the condition
        self.emit_byte(OpCode::Pop as u8, true);
        self.statement();

        // All jumps that shuold jump over the whole thing (end of any block that
        // isn't the else block. The first one is the jump after the then block).
        let mut to_end_jumps: Vec<usize> = vec![self.emit_jump(OpCode::Jump)];
        self.patch_jump(over_then_jump);
        self.emit_byte(OpCode::Pop as u8, true);

        // Else if
        while self.match_(TokenType::Elseif) && !self.match_(TokenType::Eof) {
            self.expression();
            let over_then_jump = self.emit_jump(OpCode::JumpIfFalse);
            self.emit_byte(OpCode::Pop as u8, true);
            self.statement();
            to_end_jumps.push(self.emit_jump(OpCode::Jump));
            self.patch_jump(over_then_jump);
            self.emit_byte(OpCode::Pop as u8, true);
        }

        // Pop the condition if we went to the else block
        if self.match_(TokenType::Else) {
            self.statement();
        }
        // At the end of the then block, jump over the end of the else block (if it's there)
        for jump in to_end_jumps {
            self.patch_jump(jump);
        }
    }

    fn return_statement(&mut self) {
        self.consume(
            TokenType::Return,
            "Expected 'return' to start return statement",
        );
        if self.match_(TokenType::Semicolon) {
            self.emit_bytes(OpCode::Null as u8, OpCode::Return as u8);
            return;
        }
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after return value");
        self.emit_byte(OpCode::Return as u8, true);
    }

    fn match_expr(&mut self) {
        self.expression();
        self.consume(TokenType::LeftBrace, "Expected '{' after match expression");

        let mut to_next_case: usize = usize::MAX;
        let mut seen_default = false;
        let mut to_end_jumps: Vec<usize> = Vec::new();
        while !self.match_(TokenType::RightBrace) {
            if self.match_(TokenType::Eof) {
                self.error_at_current("Expected '}' to end match statement");
                return;
            }
            if seen_default {
                self.error_at_current("Cannot have cases after default case");
                return;
            }
            if to_next_case != usize::MAX {
                self.patch_jump(to_next_case);
                self.emit_byte(OpCode::Pop as u8, true);
            }
            // Default, always jump
            if self.match_(TokenType::Underscore) {
                seen_default = true;
                self.emit_byte(OpCode::True as u8, true);
            // Compare with == (duplicate since equal consumes the op)
            // TODO PERF: comparison ops that don't pop everything?
            } else if self.peek(-1).token_type == TokenType::BigRightArrow {
                self.emit_byte(OpCode::Duplicate as u8, true);
                self.expression();
                self.emit_byte(OpCode::Equal as u8, true);
            } else {
                self.error_at_current(
                    "Match case should be either 'default' or a simple expression",
                );
            }
            to_next_case = self.emit_jump(OpCode::JumpIfFalse);
            self.emit_byte(OpCode::Pop as u8, true); // Pop the condition (true)
            self.consume(TokenType::BigRightArrow, "Expected '=>' after match case");
            // Pop the matched expression
            self.emit_byte(OpCode::Pop as u8, true);
            if self.match_(TokenType::LeftBrace) {
                self.block();
                // Blocks have no value so make them return null (kind of arbitrary?)
                // TODO: make it possible to return values from blocks?
                self.emit_byte(OpCode::Null as u8, true);
            } else {
                self.expression();
            }
            self.consume(TokenType::Semicolon, "Expected ';' after match case");
            to_end_jumps.push(self.emit_jump(OpCode::Jump));
        }
        if to_next_case == usize::MAX {
            self.error_at_current("Match statement with no cases");
            return;
        }
        self.patch_jump(to_next_case);
        self.emit_exception("No match in match statement");
        for jump in to_end_jumps {
            self.patch_jump(jump);
        }
    }

    fn patch_loop_exit_jumps(&mut self) {
        let end_jumps = self.loop_endjumps.pop().unwrap();
        for jump in end_jumps {
            self.patch_jump(jump);
        }
    }

    fn for_statement(&mut self) {
        self.consume(TokenType::For, "Expected 'for' to start for loop");
        self.begin_scope();
        self.implicit_local_var("loop variable name");
        // The loop variable needs to actually be on the stack. emit null until it gets set a value
        self.emit_byte(OpCode::Null as u8, true);
        let loop_var_name = self.peek(1).clone();
        let loop_var_idx = self.resolve_local(&loop_var_name).unwrap().1;
        self.consume(TokenType::In, "Expected 'in' after for loop variable");
        // Hopefully this is something iterable
        self.expression();

        // The range is considered a local variable since it occupies a slot on the stack.
        // Otherwise the offsets for other locals may be incorrect
        self.compiling_funcs.last_mut().unwrap().locals.push(Local {
            name: Token::new(TokenType::Identifier, "range local".to_string(), 0),
            depth: self.scope_depth as i32,
            constant: false,
        });

        let loop_start = active_chunk!(self).code.len();
        self.loop_continue_tracker
            .push((loop_start, self.scope_depth));

        // Update the loop variable
        self.emit_byte(OpCode::Next as u8, true);
        self.emit_bytes(OpCode::SetLocal as u8, loop_var_idx);

        let exit_jump = self.emit_jump(OpCode::JumpIfNull);
        self.loop_endjumps.push(vec![exit_jump]);

        self.emit_byte(OpCode::Pop as u8, true);

        // Loop body
        self.statement();
        self.emit_loop(loop_start);

        // Loop end
        self.patch_loop_exit_jumps();
        self.end_scope(false);
        self.loop_continue_tracker.pop();
    }

    fn continue_statement(&mut self) {
        self.consume(
            TokenType::Continue,
            "Expected 'continue' to start continue statement",
        );
        self.consume(TokenType::Semicolon, "Expected ';' after 'continue'");
        if self.loop_continue_tracker.is_empty() {
            self.error_at_current("Cannot use 'continue' outside of a loop");
            return;
        }
        let (loop_start, scope_depth) = *self.loop_continue_tracker.last().unwrap();
        let locals = &self.compiling_funcs.last_mut().unwrap().locals;
        let to_pop = locals
            .iter()
            .rev()
            .take_while(|local| local.depth > scope_depth as i32)
            .count();
        for _ in 0..to_pop {
            self.emit_byte(OpCode::Pop as u8, true);
        }
        self.emit_loop(loop_start);
    }

    fn break_statement(&mut self) {
        self.consume(
            TokenType::Break,
            "Expected 'break' to start break statement",
        );
        self.consume(TokenType::Semicolon, "Expected ';' after 'break'");
        if self.loop_endjumps.is_empty() {
            self.error_at_current("Cannot use 'break' outside of a loop");
            return;
        }
        let exit_jump: usize = self.emit_jump(OpCode::Jump);
        self.loop_endjumps.last_mut().unwrap().push(exit_jump);
    }

    fn while_statement(&mut self) {
        self.consume(TokenType::While, "Expected 'while' to start while loop");
        let chunk = active_chunk!(self);
        let loop_start: usize = chunk.code.len();
        self.loop_continue_tracker
            .push((loop_start, self.scope_depth));
        self.expression();
        let exit_jump: usize = self.emit_jump(OpCode::JumpIfFalse);
        self.loop_endjumps.push(Vec::new());
        self.emit_byte(OpCode::Pop as u8, true);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        // Pop the condition value if we took the exit jump
        self.emit_byte(OpCode::Pop as u8, true);
        // If we broke out of the loop, the condition value already popped
        // So we jump to after the above pop :)
        self.patch_loop_exit_jumps();
        self.loop_continue_tracker.pop();
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self, end_func: bool) {
        self.scope_depth -= 1;
        // If this is just ending a function, we don't emit pops.
        // The local variables are instead popped in the VM at runtime with a return op.
        if end_func {
            return;
        }
        let locals = &mut self.compiling_funcs.last_mut().unwrap().locals;
        let mut pops = 0;
        while !locals.is_empty() && locals.last().unwrap().depth > self.scope_depth as i32 {
            locals.pop();
            pops += 1;
        }
        for _ in 0..pops {
            self.emit_byte(OpCode::Pop as u8, true);
        }
    }

    fn block(&mut self) {
        while !self.match_(TokenType::RightBrace) {
            if self.match_(TokenType::Eof) {
                self.error_at_current("Expected '}' to end block");
                return;
            }
            self.declaration();
        }
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after expression");
        self.emit_byte(OpCode::Pop as u8, true);
    }

    fn grouping(&mut self) {
        if self.peek(-1).token_type == TokenType::Comma
            || self.peek(-2).token_type == TokenType::BigRightArrow
            || self.peek(0).token_type == TokenType::RightParen
        {
            self.lambda_function();
            return;
        }
        self.expression();
        self.consume(TokenType::RightParen, "Expected ')' after expression");
    }

    fn hash_map(&mut self) {
        let mut count = 0;
        while !self.check(TokenType::RightBrace) {
            // Allow non-stringed keys, e.g. { cats: 5, dogs: 10 } rather than
            // { "cats": 5, "dogs": 10 }
            if self.peek(0).token_type == TokenType::Identifier {
                let lexeme = self.peek(0).lexeme.clone();
                let string = Value::String(Rc::new(RefCell::new(lexeme.chars().collect())));
                self.emit_constant(string);
                self.advance();
            } else {
                self.expression();
            }
            count += 1;
            match self.peek(0).token_type {
                TokenType::Colon => {
                    self.advance();
                    self.expression();
                }
                TokenType::Comma => {
                    self.emit_byte(OpCode::Null as u8, true);
                }
                TokenType::RightBrace => {
                    self.emit_byte(OpCode::Null as u8, true);
                    break;
                }
                _ => {
                    self.error_at_current("Expected ':' or ',' in hashmap");
                    return;
                }
            }
            if !self.match_(TokenType::Comma) {
                break;
            }
        }
        self.consume(TokenType::RightBrace, "Expected '}' after hashmap");
        self.emit_bytes(OpCode::HashMap as u8, count);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = self.rules[&self.peek(1).token_type].prefix;
        if prefix_rule.is_none() {
            self.error_at_current("Expected expression");
            return;
        }
        prefix_rule.unwrap()(self);

        while precedence <= self.rules[&self.peek(0).token_type].precedence {
            self.advance();
            let infix_rule = self.rules[&self.peek(1).token_type].infix;
            infix_rule.unwrap()(self);
        }
    }

    fn identifier_constant(&mut self) -> u8 {
        let identifier = self.peek(1).lexeme.clone();
        let value = Value::String(Rc::new(RefCell::new(identifier.chars().collect())));
        self.make_constant(value)
    }

    // TODO: Disallow redefining globals
    fn global_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expected variable name");
        let global = self.identifier_constant();
        if self.match_(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Null as u8, true);
        }
        self.emit_bytes(OpCode::DefineGlobal as u8, global);
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration",
        );
    }

    fn lambda_function(&mut self) {
        self.emit_function(
            &Token::new(TokenType::Identifier, "lambda".to_string(), 0),
            true,
        );
        self.begin_scope();
        let arity = self.parse_function_params();
        self.compiling_funcs.last_mut().unwrap().arity = arity;
        self.consume(
            TokenType::BigRightArrow,
            "Expected '=>' after lambda parameters",
        );
        if self.match_(TokenType::LeftBrace) {
            self.block();
            self.emit_bytes(OpCode::Null as u8, OpCode::Return as u8);
        } else {
            self.expression();
            self.emit_byte(OpCode::Return as u8, true);
        }
        self.end_scope(true);
        self.compiled_funcs
            .push(self.compiling_funcs.pop().unwrap());
    }

    fn parse_function_params(&mut self) -> u8 {
        let mut arity = 0;
        if !self.check(TokenType::RightParen) {
            while !self.check(TokenType::RightParen) {
                self.implicit_local_var("parameter name");
                arity += 1;
                if !self.match_(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(
            TokenType::RightParen,
            "Expected ')' after function arguments",
        );
        arity
    }

    fn emit_function(&mut self, name: &Token, is_lambda: bool) {
        let start = active_chunk!(self).code.len();
        let mut new_func = Function {
            start,
            name: name.clone(),
            chunk: Chunk::new(),
            locals: Vec::new(),
            arity: 0,
            const_idx: 0, // Initialized later
        };
        let const_idx = self.emit_constant(Value::Function(new_func.clone()));
        new_func.const_idx = const_idx as usize;
        let prev_locals = &mut self.compiling_funcs.last_mut().unwrap().locals;
        // This function becomes a local variable in the surrounding function scope
        // (Unless it's a lambda)
        if !is_lambda {
            prev_locals.push(Local {
                name: name.clone(),
                depth: self.scope_depth as i32,
                constant: false,
            });
        }
        // A function wants to be able to access itself as well
        new_func.locals.push(Local {
            name: new_func.name.clone(),
            depth: self.scope_depth as i32,
            constant: true,
        });
        self.compiling_funcs.push(new_func);
    }

    fn function_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expected function name");
        let name = self.peek(1).clone();
        self.emit_function(&name, false);
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expected '(' after function name");
        let arity = self.parse_function_params();
        self.compiling_funcs.last_mut().unwrap().arity = arity;

        self.consume(TokenType::LeftBrace, "Expected '{' before function body");
        self.block();
        self.end_scope(true);
        self.emit_bytes(OpCode::Null as u8, OpCode::Return as u8);
        self.compiled_funcs
            .push(self.compiling_funcs.pop().unwrap());
    }

    fn array(&mut self) {
        let mut count = 0;
        while !self.check(TokenType::RightBracket) {
            self.expression();
            count += 1;
            if !self.match_(TokenType::Comma) {
                break;
            }
        }
        self.consume(TokenType::RightBracket, "Expected ']' after array");
        self.emit_bytes(OpCode::Array as u8, count);
    }

    fn access(&mut self) {
        self.expression();
        self.consume(TokenType::RightBracket, "Expected ']' after index");
        let next = self.peek(0).token_type;
        match next {
            TokenType::Equal => {
                self.advance();
                self.expression();
                self.emit_byte(OpCode::AccessSet as u8, true);
            }
            TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::StarEqual
            | TokenType::SlashEqual
            | TokenType::SlashDownEqual
            | TokenType::PercentEqual => {
                self.advance();
                self.emit_byte(OpCode::Dup2 as u8, true);
                self.emit_byte(OpCode::Access as u8, true);
                self.expression();
                self.emit_compound_operator(next);
                self.emit_byte(OpCode::AccessSet as u8, true);
            }
            _ => self.emit_byte(OpCode::Access as u8, true),
        }
    }

    fn in_op(&mut self) {
        self.expression();
        self.emit_byte(OpCode::In as u8, true);
    }

    fn valid_local(&mut self, name: &Token) -> bool {
        let duplicate_exists: bool =
            self.compiling_funcs
                .last()
                .unwrap()
                .locals
                .iter()
                .any(|local| {
                    local.name.lexeme == name.lexeme && self.scope_depth as i32 == local.depth
                });
        if duplicate_exists {
            self.error_at_current("Variable with this name already declared in this scope");
            return false;
        }
        if self.compiling_funcs.last().unwrap().locals.len() >= 256 {
            self.error_at_current("Too many local variables :(");
            return false;
        }
        true
    }

    // For e.g. loop variables, parameters (they are local variables)
    fn implicit_local_var(&mut self, local_type: &str) {
        self.consume(TokenType::Identifier, &format!("Expected {}", local_type));
        let name = self.peek(1).clone();
        if !self.valid_local(&name) {
            return;
        }
        let locals = &mut self.compiling_funcs.last_mut().unwrap().locals;
        locals.push(Local {
            name,
            depth: self.scope_depth as i32,
            constant: false,
        });
    }

    fn local_var_declaration(&mut self, constant: bool) {
        self.consume(TokenType::Identifier, "Expected variable name");
        let name = self.peek(1).clone();
        if !self.valid_local(&name) {
            return;
        }
        let locals = &mut self.compiling_funcs.last_mut().unwrap().locals;
        locals.push(Local {
            name,
            depth: -1,
            constant,
        });

        if self.match_(TokenType::Equal) {
            self.expression();
        } else {
            if constant {
                self.error_at_current(
                    "Expected '=' after constant declaration (must be initialized)",
                );
            }
            self.emit_byte(OpCode::Null as u8, true);
        }
        let locals = &mut self.compiling_funcs.last_mut().unwrap().locals;
        locals.last_mut().unwrap().depth = self.scope_depth as i32;
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration",
        );
    }

    fn declaration(&mut self) {
        if self.match_(TokenType::Global) {
            // TODO: disallow this when not at global scope
            self.global_declaration();
        } else if self.match_(TokenType::Let) || self.match_(TokenType::Const) {
            let constant = self.peek(1).token_type == TokenType::Const;
            self.local_var_declaration(constant);
        } else if self.match_(TokenType::Fn) {
            self.function_declaration();
        } else {
            self.statement();
        }
    }

    fn append_functions(&mut self) {
        assert_eq!(
            self.compiling_funcs.len(),
            1,
            "Expected only one chunk (the script)"
        );
        // Put all the function code after the Eof
        let mut main_func = self.compiling_funcs.pop().unwrap();
        for func in self.compiled_funcs.iter_mut() {
            func.start = main_func.chunk.code.len();
            main_func.chunk.constants[func.const_idx] = Value::Function(func.clone());
            main_func.chunk.code.append(&mut func.chunk.code);
            main_func.chunk.lines.append(&mut func.chunk.lines);
            main_func
                .chunk
                .debug_code
                .append(&mut func.chunk.debug_code);
        }
        self.compiled_funcs = vec![main_func];
    }

    fn statement(&mut self) {
        let curr_type = self.peek(0).token_type;
        match curr_type {
            TokenType::For => self.for_statement(),
            TokenType::If => self.if_statement(),
            TokenType::Return => self.return_statement(),
            TokenType::While => self.while_statement(),
            TokenType::Break => self.break_statement(),
            TokenType::Continue => self.continue_statement(),
            TokenType::LeftBrace => {
                self.begin_scope();
                self.consume(TokenType::LeftBrace, "Expected '{' to start block");
                self.block();
                self.end_scope(false);
            }
            _ => self.expression_statement(),
        }
    }
}

pub fn compile(input: &str) -> CompilerResult {
    let mut parser = Compiler::new(input);
    while !parser.match_(TokenType::Eof) {
        parser.declaration();
    }
    parser.emit_byte(OpCode::Eof as u8, true);
    parser.append_functions();
    if parser.error {
        CompilerResult::CompileError
    } else {
        CompilerResult::Chunk(parser.compiled_funcs.pop().unwrap().chunk)
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::*;

    fn verify_bytecode(chunk: &Chunk, expected: &[u8]) {
        chunk.disassemble("test");
        assert_eq!(expected.len(), chunk.code.len(), "Length mismatch");
        for (i, byte) in expected.iter().enumerate() {
            assert_eq!(*byte, chunk.code[i], "Byte mismatch at index {}", i);
        }
    }

    fn compile_to_chunk(input: &str) -> Chunk {
        let compiled = compile(input);
        match compiled {
            CompilerResult::Chunk(chunk) => chunk,
            CompilerResult::CompileError => panic!("Compile error"),
        }
    }

    #[test]
    fn basic_expression() {
        let chunk: Chunk = compile_to_chunk("1+1;");
        let expected = [
            OpCode::Constant as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Add as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn multiple_exprs() {
        let chunk: Chunk = compile_to_chunk("1+1;2+2;");
        let expected = [
            OpCode::Constant as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Add as u8,
            OpCode::Pop as u8,
            OpCode::Constant as u8,
            2,
            OpCode::Constant as u8,
            3,
            OpCode::Add as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn muliple_exprs_multiline() {
        let chunk: Chunk = compile_to_chunk("1*8;\n7-5;");
        let expected = [
            OpCode::Constant as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Mul as u8,
            OpCode::Pop as u8,
            OpCode::Constant as u8,
            2,
            OpCode::Constant as u8,
            3,
            OpCode::Sub as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn syntax_error() {
        let compiled = compile("1 +;");
        assert_eq!(compiled, CompilerResult::CompileError);
    }

    #[test]
    fn unary_negation() {
        let chunk: Chunk = compile_to_chunk("-1;");
        let expected = [
            OpCode::Constant as u8,
            0,
            OpCode::Negate as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn parse_literals() {
        let chunk: Chunk = compile_to_chunk("true; false; null;");
        let expected = [
            OpCode::True as u8,
            OpCode::Pop as u8,
            OpCode::False as u8,
            OpCode::Pop as u8,
            OpCode::Null as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn parse_string() {
        let chunk: Chunk = compile_to_chunk("\"hello\";");
        let expected = [
            OpCode::Constant as u8,
            0,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn defining_globals() {
        let chunk: Chunk = compile_to_chunk("global a; global b = 1;");
        let expected = [
            OpCode::Null as u8,
            OpCode::DefineGlobal as u8,
            0, // Points to "a"
            OpCode::Constant as u8,
            2, // Points to 1
            OpCode::DefineGlobal as u8,
            1, // Points to "b"
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn using_globals() {
        let chunk: Chunk = compile_to_chunk("global a = 1; print(a);");
        let expected = [
            OpCode::Constant as u8,
            1,
            OpCode::DefineGlobal as u8,
            0, // Points to "a"
            OpCode::GetGlobal as u8,
            2,
            OpCode::GetGlobal as u8,
            3,
            OpCode::Constant as u8,
            4,
            OpCode::Call as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn setting_globals() {
        let chunk: Chunk = compile_to_chunk("global a = 1; a = 2; print(a);");
        let expected = [
            OpCode::Constant as u8,
            1, // Points to 1
            OpCode::DefineGlobal as u8,
            0, // Points to "a"
            OpCode::Constant as u8,
            3, // Points to 2
            OpCode::SetGlobal as u8,
            2, // Points to "a"
            OpCode::Pop as u8,
            OpCode::GetGlobal as u8,
            4,
            OpCode::GetGlobal as u8,
            5,
            OpCode::Constant as u8,
            6,
            OpCode::Call as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn declaring_locals() {
        let chunk: Chunk = compile_to_chunk("let a = 5; print(a);");
        let expected = [
            OpCode::Constant as u8,
            // Points to 5, this is actually the variable a. it just lives here.
            // quote: The temporary (value) simply _becomes_ the local variable.
            // That position on the stack IS the local variable.
            0,
            OpCode::GetGlobal as u8,
            1,
            OpCode::GetLocal as u8,
            0, // Points to "a"
            OpCode::Constant as u8,
            2,
            OpCode::Call as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn redeclaring_local() {
        let chunk: Chunk = compile_to_chunk("let a = 100; {let a = 10; print(a);}");
        let expected = [
            OpCode::Constant as u8,
            0, // outer a
            OpCode::Constant as u8,
            1, // Inner a
            OpCode::GetGlobal as u8,
            2,
            OpCode::GetLocal as u8,
            1,
            OpCode::Constant as u8,
            3,
            OpCode::Call as u8,
            OpCode::Pop as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn good_constant() {
        let chunk: Chunk = compile_to_chunk("const a = 100; print(a);");
        let expected = [
            OpCode::Constant as u8,
            0, // Points to 100
            OpCode::GetGlobal as u8,
            1,
            OpCode::GetLocal as u8,
            0, // Points to "a"
            OpCode::Constant as u8,
            2,
            OpCode::Call as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn bad_constant() {
        let compiled = compile("const a; print(a);");
        assert_eq!(compiled, CompilerResult::CompileError);
        let compiled = compile("const a = 100; a = 200;");
        assert_eq!(compiled, CompilerResult::CompileError);
    }

    #[test]
    fn if_statement() {
        let chunk: Chunk = compile_to_chunk("if (true) { print(1); }");
        let expected = [
            OpCode::True as u8,
            OpCode::JumpIfFalse as u8, // Jump 1 FROM
            0x0,
            0xc,
            OpCode::Pop as u8,
            OpCode::GetGlobal as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Constant as u8,
            2,
            OpCode::Call as u8,
            OpCode::Pop as u8,
            OpCode::Jump as u8, // Jump 2 FROM
            0x0,
            0x1,
            OpCode::Pop as u8, // Jump 1 TO
            OpCode::Eof as u8, // Jump 2 TO
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn if_else_statement() {
        let chunk: Chunk = compile_to_chunk("if (true) { print(1); } else { print(2); }");
        let expected = [
            OpCode::True as u8,
            OpCode::JumpIfFalse as u8, // Jump 1 FROM
            0x0,
            0xc,
            OpCode::Pop as u8,
            OpCode::GetGlobal as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Constant as u8,
            2,
            OpCode::Call as u8,
            OpCode::Pop as u8,
            OpCode::Jump as u8, // Jump 2 FROM
            0x0,
            0x9,
            OpCode::Pop as u8, // Jump 1 TO
            OpCode::GetGlobal as u8,
            3,
            OpCode::Constant as u8,
            4,
            OpCode::Constant as u8,
            5,
            OpCode::Call as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8, // Jump 2 TO
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn bad_if_statements() {
        let compiled = compile("else { print(2); }");
        assert_eq!(compiled, CompilerResult::CompileError);
        let compiled = compile("if print({ print(2); })");
        assert_eq!(compiled, CompilerResult::CompileError);
    }

    #[test]
    fn else_if_statements() {
        let chunk: Chunk =
            compile_to_chunk("if (true) { print(1); } else if (false) { print(2); }");
        let expected = [
            OpCode::True as u8,
            OpCode::JumpIfFalse as u8,
            0x0,
            0xc,
            OpCode::Pop as u8,
            OpCode::GetGlobal as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Constant as u8,
            2,
            OpCode::Call as u8,
            OpCode::Pop as u8,
            OpCode::Jump as u8,
            0x0,
            0x12,
            OpCode::Pop as u8,
            OpCode::False as u8,
            OpCode::JumpIfFalse as u8,
            0x0,
            0xc,
            OpCode::Pop as u8,
            OpCode::GetGlobal as u8,
            3,
            OpCode::Constant as u8,
            4,
            OpCode::Constant as u8,
            5,
            OpCode::Call as u8,
            OpCode::Pop as u8,
            OpCode::Jump as u8,
            0x0,
            0x1,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn while_statement() {
        let chunk: Chunk = compile_to_chunk("let a = 0;\n while a < 10 {\n a = a + 1; \n}");
        let expected = [
            OpCode::Constant as u8,
            0,
            OpCode::GetLocal as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::GreaterEqual as u8,
            OpCode::Not as u8,
            OpCode::JumpIfFalse as u8,
            0x0,
            0xc,
            OpCode::Pop as u8,
            OpCode::GetLocal as u8,
            0,
            OpCode::Constant as u8,
            2,
            OpCode::Add as u8,
            OpCode::SetLocal as u8,
            0,
            OpCode::Pop as u8,
            OpCode::Loop as u8,
            0x0,
            0x15,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn compiles_functions() {
        compile_to_chunk("fn a() { print(1); }");
        compile_to_chunk("fn f(a, b, c) { return a + b + c; }");
        compile_to_chunk("fn f(a, b, c) { fn g() { return 1; } }");
    }

    #[test]
    fn can_construct_ranges() {
        let chunk: Chunk = compile_to_chunk("1..10;");
        let expected = [
            OpCode::Constant as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Null as u8,
            OpCode::Range as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn can_compile_compound_assignment() {
        let chunk: Chunk = compile_to_chunk("let a = 1; a += 1;");
        let expected = [
            OpCode::Constant as u8,
            0,
            OpCode::GetLocal as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Add as u8,
            OpCode::SetLocal as u8,
            0,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn can_compile_both_expression_and_block_lambdas() {
        compile_to_chunk("let a = () => 1; let b = (x) => x * 5;");
        compile_to_chunk("let a = () => {print(1);}; let b = (x) => {print(x);};");
    }

    #[test]
    fn no_weird_statement_lambdas() {
        let compiled = compile("let a = () => if true {print(x)};");
        assert_eq!(compiled, CompilerResult::CompileError);
    }

    #[test]
    fn basic_match() {
        let compiled = compile("match 1 { 2 => 3; };");
        assert_ne!(compiled, CompilerResult::CompileError);
    }

    #[test]
    fn no_dumb_matches() {
        let compiled = compile("match 1 { x+5 => 10;};");
        assert_eq!(compiled, CompilerResult::CompileError);
    }

    #[test]
    fn parses_hashmap() {
        let compiled = compile("let x = {1: 2, 3: 4};");
        assert_ne!(compiled, CompilerResult::CompileError);
    }

    #[test]
    fn parses_set() {
        let compiled = compile("let x = {1, 2, 3};");
        assert_ne!(compiled, CompilerResult::CompileError);
    }

    #[test]
    fn parses_mixed_hm_and_set() {
        let compiled = compile("let x = {1, 2, 3, 4: 5};");
        assert_ne!(compiled, CompilerResult::CompileError);
    }

    #[test]
    fn compiles_in_op_correctly() {
        let chunk: Chunk = compile_to_chunk("1 in [1, 2, 3];");
        let expected = [
            OpCode::Constant as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Constant as u8,
            2,
            OpCode::Constant as u8,
            3,
            OpCode::Array as u8,
            3,
            OpCode::In as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }

    #[test]
    fn compiles_ternary() {
        let chunk: Chunk = compile_to_chunk("let x = true; x ? 1 : 2;");
        let expected = [
            OpCode::True as u8,
            OpCode::GetLocal as u8,
            0,
            OpCode::JumpIfFalse as u8,
            0x0,
            0x6,
            OpCode::Pop as u8,
            OpCode::Constant as u8,
            0,
            OpCode::Jump as u8,
            0x0,
            0x3,
            OpCode::Pop as u8,
            OpCode::Constant as u8,
            1,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        verify_bytecode(&chunk, &expected);
    }
}
