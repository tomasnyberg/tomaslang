use std::{collections::HashMap, ops::Shr};

use crate::{chunk::*, scanner::*, vm::Value};

macro_rules! active_chunk {
    ($this:expr) => {
        &mut $this.compiling_chunks.last_mut().unwrap()
    };
}

struct Compiler {
    tokens: Vec<Token>,
    current: usize,
    error: bool,
    compiling_chunks: Vec<Chunk>,
    compiled_chunks: Vec<Chunk>,
    rules: HashMap<TokenType, ParseRule>,
    locals: Vec<Local>,
    scope_depth: usize,
    functions: Vec<Function>,
}

struct Local {
    name: Token,
    depth: i32,
    constant: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub start: usize,
    pub name: Token,
    arity: u8,
    const_idx: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
#[allow(dead_code)]
pub enum OpCode {
    Constant,
    Add,
    Sub,
    Mul,
    Div,
    Equal,
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
    Jump,
    Loop,
    Call,
    Negate,
    Not,
    Pop,
    Print,
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
            1 => OpCode::Add,
            2 => OpCode::Sub,
            3 => OpCode::Mul,
            4 => OpCode::Div,
            5 => OpCode::Equal,
            6 => OpCode::NotEqual,
            7 => OpCode::Greater,
            8 => OpCode::GreaterEqual,
            9 => OpCode::DefineGlobal,
            10 => OpCode::GetGlobal,
            11 => OpCode::SetGlobal,
            12 => OpCode::GetLocal,
            13 => OpCode::SetLocal,
            14 => OpCode::JumpIfFalse,
            15 => OpCode::JumpIfTrue,
            16 => OpCode::Jump,
            17 => OpCode::Loop,
            18 => OpCode::Call,
            19 => OpCode::Negate,
            20 => OpCode::Not,
            21 => OpCode::Pop,
            22 => OpCode::Print,
            23 => OpCode::Null,
            24 => OpCode::True,
            25 => OpCode::False,
            26 => OpCode::Return,
            27 => OpCode::Eof,
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
        rule(Comma,        None,                  None,               Precedence::None);
        rule(Dot,          None,                  None,               Precedence::None);
        rule(Minus,        Some(Self::unary),     Some(Self::binary), Precedence::Term);
        rule(Plus,         None,                  Some(Self::binary), Precedence::Term);
        rule(Semicolon,    None,                  None,               Precedence::None);
        rule(Slash,        None,                  Some(Self::binary), Precedence::Factor);
        rule(Star,         None,                  Some(Self::binary), Precedence::Factor);
        rule(Bang,         Some(Self::unary),     None,               Precedence::None);
        rule(BangEqual,    None,                  Some(Self::binary), Precedence::Equality);
        rule(Equal,        None,                  None,               Precedence::None);
        rule(EqualEqual,   None,                  Some(Self::binary), Precedence::Equality);
        rule(Greater,      None,                  Some(Self::binary), Precedence::Comparison);
        rule(GreaterEqual, None,                  Some(Self::binary), Precedence::Comparison);
        rule(Less,         None,                  Some(Self::binary), Precedence::Comparison);
        rule(LessEqual,    None,                  Some(Self::binary), Precedence::Comparison);
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

        Self {
            tokens,
            current: 0,
            error: false,
            compiling_chunks: vec![Chunk::new()],
            compiled_chunks: Vec::new(),
            rules: Self::create_rules(),
            locals: Vec::new(),
            scope_depth: 0,
            functions: Vec::new(),
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) {
        if self.tokens[self.current].token_type == token_type {
            self.advance();
            return;
        }
        self.error_at_current(message);
    }

    fn error_at_current(&mut self, message: &str) {
        if self.error {
            return;
        }
        self.error = true;
        let token = &self.tokens[self.current];
        eprintln!(
            "[line {}] Error at '{}': {}",
            token.line, token.lexeme, message
        );
    }

    fn match_(&mut self, token_type: TokenType) -> bool {
        if self.tokens[self.current].token_type == token_type {
            self.advance();
            return true;
        }
        false
    }

    fn call(&mut self) {
        // TODO: Arg count
        self.emit_byte(OpCode::Call as u8, true);
        self.consume(TokenType::RightParen, "Expected ')' after arguments");
    }

    fn literal(&mut self) {
        match self.tokens[self.current - 1].token_type {
            TokenType::Null => self.emit_byte(OpCode::Null as u8, true),
            TokenType::True => self.emit_byte(OpCode::True as u8, true),
            TokenType::False => self.emit_byte(OpCode::False as u8, true),
            _ => self.error_at_current("Expected literal (Null, True, False)"),
        }
    }

    fn resolve_local(&mut self, name: &Token) -> Option<u8> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if name.lexeme == local.name.lexeme {
                if local.depth == -1 {
                    self.error_at_current("Cannot read local variable in its own initializer");
                }
                return Some(i as u8);
            }
        }
        None
    }

    // TODO: Disallow assigning in weird ways e.g a + (b = 1)
    fn variable(&mut self) {
        let var_token = self.tokens[self.current - 1].clone();
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
        if self.match_(TokenType::Equal) {
            if resolved.is_some() && self.locals[arg as usize].constant {
                self.error_at_current("Cannot assign to constant variable");
            }
            self.expression();
            self.emit_bytes(set_op as u8, arg);
        } else {
            self.emit_bytes(get_op as u8, arg);
        }
    }

    fn unary(&mut self) {
        let operator = self.tokens[self.current - 1].token_type;
        self.parse_precedence(Precedence::Unary);
        match operator {
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8, true),
            TokenType::Bang => self.emit_byte(OpCode::Not as u8, true),
            _ => self.error_at_current("Expected unary operator"),
        }
    }

    fn emit_jump(&mut self, jump_type: OpCode) -> usize {
        assert!([OpCode::Jump, OpCode::JumpIfFalse, OpCode::JumpIfTrue].contains(&jump_type));
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
        let first_chunk = &mut self.compiling_chunks[0];
        let index: u8 = first_chunk.constants.len() as u8;
        first_chunk.constants.push(value);
        if first_chunk.constants.len() > 256 {
            self.error_at_current("Too many constants in one chunk");
        }
        index
    }

    fn string(&mut self) {
        if self.tokens[self.current - 1].token_type == TokenType::String {
            let string = self.tokens[self.current - 1].lexeme.clone();
            self.emit_constant(Value::String(string));
            return;
        }
        self.error_at_current("Expected string");
    }

    fn number(&mut self) {
        if self.tokens[self.current - 1].token_type == TokenType::Number {
            let number = self.tokens[self.current - 1].lexeme.parse::<f64>().unwrap();
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
        let chunk = active_chunk!(self);
        chunk.code.push(byte);
        chunk
            .lines
            .push(self.tokens[self.current - 1].line as usize);
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

    fn binary(&mut self) {
        let operator = self.tokens[self.current - 1].token_type;
        let rule: ParseRule = self.rules[&operator];
        self.parse_precedence(rule.precedence.next());
        match operator {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8, true),
            TokenType::Minus => self.emit_byte(OpCode::Sub as u8, true),
            TokenType::Star => self.emit_byte(OpCode::Mul as u8, true),
            TokenType::Slash => self.emit_byte(OpCode::Div as u8, true),
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal as u8, true),
            TokenType::BangEqual => self.emit_byte(OpCode::NotEqual as u8, true),
            TokenType::Greater => self.emit_byte(OpCode::Greater as u8, true),
            TokenType::GreaterEqual => self.emit_byte(OpCode::GreaterEqual as u8, true),
            // TODO PERF: dedicated ops for these
            TokenType::Less => self.emit_bytes(OpCode::GreaterEqual as u8, OpCode::Not as u8),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            _ => self.error_at_current("Expected binary operator"),
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn print_statement(&mut self) {
        self.advance(); // Move over the print token
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after value");
        self.emit_byte(OpCode::Print as u8, true);
    }

    fn for_statement(&mut self) {
        panic!("For statement not implemented");
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
        panic!("Return statement not implemented");
    }

    fn while_statement(&mut self) {
        self.consume(TokenType::While, "Expected 'while' to start while loop");
        let chunk = active_chunk!(self);
        let loop_start: usize = chunk.code.len();
        self.expression();
        let exit_jump: usize = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop as u8, true);
        self.statement();
        self.emit_loop(loop_start);
        self.patch_jump(exit_jump);
        self.emit_byte(OpCode::Pop as u8, true);
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth as i32
        {
            self.emit_byte(OpCode::Pop as u8, true);
            self.locals.pop();
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
        let prefix_rule = self.rules[&self.tokens[self.current - 1].token_type].prefix;
        if prefix_rule.is_none() {
            // TODO: Erroring at the correct token?
            self.error_at_current("Expected expression");
            return;
        }
        prefix_rule.unwrap()(self);

        while precedence <= self.rules[&self.tokens[self.current].token_type].precedence {
            self.advance();
            let infix_rule = self.rules[&self.tokens[self.current - 1].token_type].infix;
            infix_rule.unwrap()(self);
        }
    }

    fn identifier_constant(&mut self) -> u8 {
        let identifier = self.tokens[self.current - 1].lexeme.clone();
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
        let name = self.tokens[self.current - 1].clone();
        let start = active_chunk!(self).code.len();
        self.functions.push(Function {
            start,
            name: name.clone(),
            arity: 0,     // TODO: arity
            const_idx: 0, // Initialized later
        });
        self.locals.push(Local {
            name,
            depth: self.scope_depth as i32,
            constant: false,
        });
        self.compiling_chunks.push(Chunk::new());
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expected '(' after function name");
        self.consume(
            TokenType::RightParen,
            "Expected ')' after function arguments",
        );
        self.consume(TokenType::LeftBrace, "Expected '{' before function body");
        self.block();
        self.end_scope();
        // TODO: Allow return value
        self.emit_bytes(OpCode::Null as u8, OpCode::Return as u8);
        self.compiled_chunks
            .push(self.compiling_chunks.pop().unwrap());
        let const_idx = self.emit_constant(Value::Function(self.functions.last().unwrap().clone()));
        self.functions.last_mut().unwrap().const_idx = const_idx as usize;
    }

    fn local_var_declaration(&mut self, constant: bool) {
        self.consume(TokenType::Identifier, "Expected variable name");
        let name = self.tokens[self.current - 1].clone();
        for local in self.locals.iter().rev() {
            if local.depth != self.scope_depth as i32 {
                break;
            }
            if name.lexeme == local.name.lexeme {
                self.error_at_current("Variable with this name already declared in this scope");
                break;
            }
        }
        if self.locals.len() == 256 {
            self.error_at_current("Too many local variables :(");
            return;
        }
        self.locals.push(Local {
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
        self.locals.last_mut().unwrap().depth = self.scope_depth as i32;
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
            let constant = self.tokens[self.current - 1].token_type == TokenType::Const;
            self.local_var_declaration(constant);
        } else if self.match_(TokenType::Fn) {
            self.function_declaration();
        } else {
            self.statement();
        }
    }

    fn append_functions(&mut self) {
        assert_eq!(
            self.compiling_chunks.len(),
            1,
            "Expected only one chunk (the script)"
        );
        // Put all the function code after the Eof
        let mut final_chunk = self.compiling_chunks.pop().unwrap();
        for (chunk, func) in self
            .compiled_chunks
            .iter_mut()
            .zip(self.functions.iter_mut())
        {
            func.start = final_chunk.code.len();
            final_chunk.constants[func.const_idx] = Value::Function(func.clone());
            final_chunk.code.append(&mut chunk.code);
            final_chunk.lines.append(&mut chunk.lines);
            final_chunk.debug_code.append(&mut chunk.debug_code);
        }
        self.compiled_chunks = vec![final_chunk];
    }

    fn statement(&mut self) {
        let curr_type = self.tokens[self.current].token_type;
        // TODO: Remember to move past the token (except for in expression)
        match curr_type {
            TokenType::Print => self.print_statement(),
            TokenType::For => self.for_statement(),
            TokenType::If => self.if_statement(),
            TokenType::Return => self.return_statement(),
            TokenType::While => self.while_statement(),
            TokenType::LeftBrace => {
                self.begin_scope();
                self.consume(TokenType::LeftBrace, "Expected '{' to start block");
                self.block();
                self.end_scope();
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
        CompilerResult::Chunk(parser.compiled_chunks.pop().unwrap())
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
}
