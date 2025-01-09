use std::collections::HashMap;
use std::ops::Shr;

use crate::{chunk::*, scanner::*, vm::Value};

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

#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub start: i32,
    pub end: i32,
    pub current: i32,
    pub step: i32,
}

impl Range {
    pub fn new(start: i32, end: i32) -> Self {
        Self {
            start,
            end,
            current: start,
            step: if start < end { 1 } else { -1 },
        }
    }

    pub fn next(&mut self) -> Option<i32> {
        if self.current == self.end {
            return None;
        }
        let current = self.current;
        self.current += self.step;
        Some(current)
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
        format!("R {}..{}", lower, upper)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
#[allow(dead_code)]
pub enum OpCode {
    Constant,
    Array,
    Add,
    Sub,
    Mul,
    Div,
    DivInt,
    Equal,
    Mod,
    NotEqual,
    Greater,
    GreaterEqual,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
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
    Print,
    Range,
    Null,
    True,
    False,
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
            1 => OpCode::Array,
            2 => OpCode::Add,
            3 => OpCode::Sub,
            4 => OpCode::Mul,
            5 => OpCode::Div,
            6 => OpCode::DivInt,
            7 => OpCode::Equal,
            8 => OpCode::Mod,
            9 => OpCode::NotEqual,
            10 => OpCode::Greater,
            11 => OpCode::GreaterEqual,
            12 => OpCode::DefineGlobal,
            13 => OpCode::GetGlobal,
            14 => OpCode::SetGlobal,
            15 => OpCode::GetLocal,
            16 => OpCode::SetLocal,
            17 => OpCode::JumpIfFalse,
            18 => OpCode::JumpIfTrue,
            19 => OpCode::JumpIfNull,
            20 => OpCode::Jump,
            21 => OpCode::Loop,
            22 => OpCode::Call,
            23 => OpCode::Negate,
            24 => OpCode::Not,
            25 => OpCode::Next,
            26 => OpCode::Pop,
            27 => OpCode::Print,
            28 => OpCode::Range,
            29 => OpCode::Null,
            30 => OpCode::True,
            31 => OpCode::False,
            32 => OpCode::Return,
            33 => OpCode::Eof,
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
        rule(LeftParen,    Some(Self::grouping),  Some(Self::call),   Precedence::Call);
        rule(RightParen,   None,                  None,               Precedence::None);
        rule(LeftBrace,    None,                  None,               Precedence::None);
        rule(RightBrace,   None,                  None,               Precedence::None);
        rule(LeftBracket,  Some(Self::array),     None,               Precedence::Call);
        rule(RightBracket, None,                  None,               Precedence::None);
        rule(Comma,        None,                  None,               Precedence::None);
        rule(Dot,          None,                  None,               Precedence::None);
        // Is precedence correct here? Not sure.
        rule(DotDot,       None,                  Some(Self::range),  Precedence::Term);
        rule(Minus,        Some(Self::unary),     Some(Self::binary), Precedence::Term);
        rule(Plus,         None,                  Some(Self::binary), Precedence::Term);
        rule(Semicolon,    None,                  None,               Precedence::None);
        rule(Slash,        None,                  Some(Self::binary), Precedence::Factor);
        rule(SlashDown,    None,                  Some(Self::binary), Precedence::Factor);
        rule(Star,         None,                  Some(Self::binary), Precedence::Factor);
        rule(Bang,         Some(Self::unary),     None,               Precedence::None);
        rule(BangEqual,    None,                  Some(Self::binary), Precedence::Equality);
        rule(Equal,        None,                  None,               Precedence::None);
        rule(EqualEqual,   None,                  Some(Self::binary), Precedence::Equality);
        rule(Greater,      None,                  Some(Self::binary), Precedence::Comparison);
        rule(GreaterEqual, None,                  Some(Self::binary), Precedence::Comparison);
        rule(Less,         None,                  Some(Self::binary), Precedence::Comparison);
        rule(LessEqual,    None,                  Some(Self::binary), Precedence::Comparison);
        rule(Percent,      None,                  Some(Self::binary), Precedence::Factor);
        rule(Identifier,   Some(Self::variable),  None,               Precedence::None);
        rule(String,       Some(Self::string),    None,               Precedence::None);
        rule(Number,       Some(Self::number),    None,               Precedence::None);
        rule(And,          None,                  Some(Self::and),    Precedence::And);
        rule(Class,        None,                  None,               Precedence::None);
        rule(Else,         None,                  None,               Precedence::None);
        rule(False,        Some(Self::literal),   None,               Precedence::None);
        rule(Fn,           None,                  None,               Precedence::None);
        rule(For,          None,                  None,               Precedence::None);
        rule(If,           None,                  None,               Precedence::None);
        rule(Null,         Some(Self::literal),   None,               Precedence::None);
        rule(Or,           None,                  Some(Self::or),     Precedence::Or);
        rule(Print,        None,                  None,               Precedence::None);
        rule(Return,       None,                  None,               Precedence::None);
        rule(Super,        None,                  None,               Precedence::None);
        rule(This,         None,                  None,               Precedence::None);
        rule(True,         Some(Self::literal),   None,               Precedence::None);
        rule(Let,          None,                  None,               Precedence::None);
        rule(While,        None,                  None,               Precedence::None);
        rule(Error,        None,                  None,               Precedence::None);
        rule(Eof,          None,                  None,               Precedence::None);

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

    fn literal(&mut self) {
        match self.peek(1).token_type {
            TokenType::Null => self.emit_byte(OpCode::Null as u8, true),
            TokenType::True => self.emit_byte(OpCode::True as u8, true),
            TokenType::False => self.emit_byte(OpCode::False as u8, true),
            _ => self.error_at_current("Expected literal (Null, True, False)"),
        }
    }

    fn resolve_local(&mut self, name: &Token) -> Option<u8> {
        let locals = &self.compiling_funcs.last().unwrap().locals;
        for (i, local) in locals.iter().enumerate().rev() {
            if name.lexeme == local.name.lexeme {
                if local.depth == -1 {
                    self.error_at_current("Cannot read local variable in its own initializer");
                }
                return Some(i as u8);
            }
        }
        None
    }

    fn ensure_not_const(&mut self, resolved: Option<u8>, arg: usize) {
        if resolved.is_some() && self.compiling_funcs.last().unwrap().locals[arg].constant {
            self.error_at_current("Cannot assign to constant variable");
        }
    }

    // TODO: Disallow assigning in weird ways e.g a + (b = 1)
    fn variable(&mut self) {
        let var_token = self.peek(1).clone();
        let set_op: OpCode;
        let get_op: OpCode;
        let arg: u8;
        let resolved = self.resolve_local(&var_token);
        if resolved.is_some() {
            arg = resolved.unwrap();
            set_op = OpCode::SetLocal;
            get_op = OpCode::GetLocal;
        } else {
            arg = self.identifier_constant();
            set_op = OpCode::SetGlobal;
            get_op = OpCode::GetGlobal;
        }
        let operator = self.peek(0).token_type;
        match operator {
            TokenType::Equal => {
                self.advance();
                self.ensure_not_const(resolved, arg as usize);
                self.expression();
                self.emit_bytes(set_op as u8, arg);
            }
            TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::StarEqual
            | TokenType::SlashEqual
            | TokenType::SlashDownEqual
            | TokenType::PercentEqual
            | TokenType::ColonEqual => {
                self.advance();
                self.ensure_not_const(resolved, arg as usize);
                self.emit_bytes(get_op as u8, arg);
                self.expression();
                self.emit_compound_operator(operator);
                self.emit_bytes(set_op as u8, arg);
            }
            _ => self.emit_bytes(get_op as u8, arg),
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
            self.emit_constant(Value::String(string));
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

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn print_statement(&mut self) {
        self.consume(
            TokenType::Print,
            "Expected 'print' to start print statement",
        );
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after value");
        self.emit_byte(OpCode::Print as u8, true);
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

    fn patch_loop_exit_jumps(&mut self) {
        let end_jumps = self.loop_endjumps.pop().unwrap();
        for jump in end_jumps {
            self.patch_jump(jump);
        }
    }

    fn for_statement(&mut self) {
        self.consume(TokenType::For, "Expected 'for' to start for loop");
        self.begin_scope();
        self.local_var_declaration(false, true); // Loop variable
        let loop_var_name = self.peek(1).clone();
        let loop_var_idx = self.resolve_local(&loop_var_name).unwrap();
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
        self.expression();
        self.consume(TokenType::RightParen, "Expected ')' after expression");
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
        let value = Value::String(identifier);
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

    fn function_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expected function name");
        let name = self.peek(1).clone();
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
        // This function becomes a local variable in the surrounding function scope
        let prev_locals = &mut self.compiling_funcs.last_mut().unwrap().locals;
        prev_locals.push(Local {
            name,
            depth: self.scope_depth as i32, // Is this correct?
            constant: false,
        });
        // A function wants to be able to access itself as well
        new_func.locals.push(Local {
            name: new_func.name.clone(),
            depth: self.scope_depth as i32,
            constant: true,
        });
        self.compiling_funcs.push(new_func);
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expected '(' after function name");
        if !self.check(TokenType::RightParen) {
            let mut arity = 0;
            while !self.check(TokenType::RightParen) {
                self.local_var_declaration(false, true);
                arity += 1;
                if !self.match_(TokenType::Comma) {
                    break;
                }
            }
            self.compiling_funcs.last_mut().unwrap().arity = arity;
        }
        self.consume(
            TokenType::RightParen,
            "Expected ')' after function arguments",
        );
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

    fn local_var_declaration(&mut self, constant: bool, no_semicolon: bool) {
        self.consume(TokenType::Identifier, "Expected variable name");
        let name = self.peek(1).clone();
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
        }
        let locals = &mut self.compiling_funcs.last_mut().unwrap().locals;
        if locals.len() == 256 {
            self.error_at_current("Too many local variables :(");
            return;
        }
        // TODO: We should not push this if a duplicate was found?
        locals.push(Local {
            name,
            depth: -1, // Maybe -1 to avoid let a = a?
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
        if no_semicolon {
            return;
        }
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
            self.local_var_declaration(constant, false);
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
            TokenType::Print => self.print_statement(),
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

    fn match_bytecode(chunk: &Chunk, expected: &[u8]) {
        assert_eq!(expected.len(), chunk.code.len());
        for (i, byte) in expected.iter().enumerate() {
            assert_eq!(*byte, chunk.code[i]);
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
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
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
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
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
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
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
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
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
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
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
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
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
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }

    #[test]
    fn using_globals() {
        let chunk: Chunk = compile_to_chunk("global a = 1; print a;");
        let expected = [
            OpCode::Constant as u8,
            1,
            OpCode::DefineGlobal as u8,
            0, // Points to "a"
            OpCode::GetGlobal as u8,
            2, // Points to "a"
            OpCode::Print as u8,
            OpCode::Eof as u8,
        ];
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }

    #[test]
    fn setting_globals() {
        let chunk: Chunk = compile_to_chunk("global a = 1; a = 2; print a;");
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
            4, // Points to "a"
            OpCode::Print as u8,
            OpCode::Eof as u8,
        ];
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }

    #[test]
    fn declaring_locals() {
        let chunk: Chunk = compile_to_chunk("let a = 5; print a;");
        let expected = [
            OpCode::Constant as u8,
            // Points to 5, this is actually the variable a. it just lives here.
            // quote: The temporary (value) simply _becomes_ the local variable.
            // That position on the stack IS the local variable.
            0,
            OpCode::GetLocal as u8,
            0, // Points to "a"
            OpCode::Print as u8,
            OpCode::Eof as u8,
        ];
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }

    #[test]
    fn redeclaring_local() {
        let chunk: Chunk = compile_to_chunk("let a = 100; {let a = 10; print a;}");
        let expected = [
            OpCode::Constant as u8,
            0, // outer a
            OpCode::Constant as u8,
            1, // Inner a
            OpCode::GetLocal as u8,
            1, // inner a should be printed
            OpCode::Print as u8,
            OpCode::Pop as u8, // End the scope
            OpCode::Eof as u8,
        ];
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }

    #[test]
    fn good_constant() {
        let chunk: Chunk = compile_to_chunk("const a = 100; print a;");
        let expected = [
            OpCode::Constant as u8,
            0, // Points to 100
            OpCode::GetLocal as u8,
            0, // Points to "a"
            OpCode::Print as u8,
            OpCode::Eof as u8,
        ];
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }

    #[test]
    fn bad_constant() {
        let compiled = compile("const a; print a;");
        assert_eq!(compiled, CompilerResult::CompileError);
        let compiled = compile("const a = 100; a = 200;");
        assert_eq!(compiled, CompilerResult::CompileError);
    }

    #[test]
    fn if_statement() {
        let chunk: Chunk = compile_to_chunk("if (true) { print 1; }");
        let expected = [
            OpCode::True as u8,
            OpCode::JumpIfFalse as u8, // Jump 1 FROM
            0x0,
            0x7,
            OpCode::Pop as u8,
            OpCode::Constant as u8,
            0,
            OpCode::Print as u8,
            OpCode::Jump as u8, // Jump 2 FROM
            0x0,
            0x1,
            OpCode::Pop as u8, // Jump 1 TO
            OpCode::Eof as u8, // Jump 2 TO
        ];
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }

    #[test]
    fn if_else_statement() {
        let chunk: Chunk = compile_to_chunk("if (true) { print 1; } else { print 2; }");
        let expected = [
            OpCode::True as u8,
            OpCode::JumpIfFalse as u8, // Jump 1 FROM
            0x0,
            0x7,
            OpCode::Pop as u8,
            OpCode::Constant as u8,
            0,
            OpCode::Print as u8,
            OpCode::Jump as u8, // Jump 2 FROM
            0x0,
            0x4,
            OpCode::Pop as u8, // Jump 1 TO
            OpCode::Constant as u8,
            1,
            OpCode::Print as u8,
            OpCode::Eof as u8, // Jump 2 TO
        ];
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }

    #[test]
    fn bad_if_statements() {
        let compiled = compile("else { print 2; }");
        assert_eq!(compiled, CompilerResult::CompileError);
        let compiled = compile("if print { print 2; }");
        assert_eq!(compiled, CompilerResult::CompileError);
    }

    #[test]
    fn else_if_statements() {
        let chunk: Chunk = compile_to_chunk("if (true) { print 1; } else if (false) { print 2; }");
        let expected = [
            OpCode::True as u8,
            OpCode::JumpIfFalse as u8,
            0x0,
            0x7,
            OpCode::Pop as u8,
            OpCode::Constant as u8,
            0,
            OpCode::Print as u8,
            OpCode::Jump as u8,
            0x0,
            0xd,
            OpCode::Pop as u8,
            OpCode::False as u8,
            OpCode::JumpIfFalse as u8,
            0x0,
            0x7,
            OpCode::Pop as u8,
            OpCode::Constant as u8,
            1,
            OpCode::Print as u8,
            OpCode::Jump as u8,
            0x0,
            0x1,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
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
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }

    #[test]
    fn compiles_functions() {
        compile_to_chunk("fn a() { print 1; }");
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
            OpCode::Range as u8,
            OpCode::Pop as u8,
            OpCode::Eof as u8,
        ];
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
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
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }
}
