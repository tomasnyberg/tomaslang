use std::collections::HashMap;

use crate::{chunk::*, scanner::*, vm::Value};

struct Compiler {
    tokens: Vec<Token>,
    current: usize,
    error: bool,
    chunk: Chunk,
    rules: HashMap<TokenType, ParseRule>,
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
#[allow(dead_code)]
pub enum OpCode {
    Constant,
    Add,
    Sub,
    Mul,
    Div,
    DefineGlobal,
    GetGlobal,
    Negate,
    Not,
    Pop,
    Print,
    Null,
    True,
    False,
    Return,
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
            5 => OpCode::DefineGlobal,
            6 => OpCode::GetGlobal,
            7 => OpCode::Negate,
            8 => OpCode::Not,
            9 => OpCode::Pop,
            10 => OpCode::Print,
            11 => OpCode::Null,
            12 => OpCode::True,
            13 => OpCode::False,
            14 => OpCode::Return,
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
        rule(BangEqual,    None,                  None,               Precedence::Equality);
        rule(Equal,        None,                  None,               Precedence::None);
        rule(EqualEqual,   None,                  None,               Precedence::Equality);
        rule(Greater,      None,                  None,               Precedence::Comparison);
        rule(GreaterEqual, None,                  None,               Precedence::Comparison);
        rule(Less,         None,                  None,               Precedence::Comparison);
        rule(LessEqual,    None,                  None,               Precedence::Comparison);
        rule(Identifier,   None,                  None,               Precedence::None);
        rule(String,       Some(Self::string),    None,               Precedence::None);
        rule(Number,       Some(Self::number),    None,               Precedence::None);
        rule(And,          None,                  None,               Precedence::And);
        rule(Class,        None,                  None,               Precedence::None);
        rule(Else,         None,                  None,               Precedence::None);
        rule(False,        Some(Self::literal),   None,               Precedence::None);
        rule(Fun,          None,                  None,               Precedence::None);
        rule(For,          None,                  None,               Precedence::None);
        rule(If,           None,                  None,               Precedence::None);
        rule(Null,         Some(Self::literal),   None,               Precedence::None);
        rule(Or,           None,                  None,               Precedence::Or);
        rule(Print,        None,                  None,               Precedence::None);
        rule(Return,       None,                  None,               Precedence::None);
        rule(Super,        None,                  None,               Precedence::None);
        rule(This,         None,                  None,               Precedence::None);
        rule(True,         Some(Self::literal),   None,               Precedence::None);
        rule(Var,          None,                  None,               Precedence::None);
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
            chunk: Chunk::new(),
            rules: Self::create_rules(),
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
        self.error_at_current("Call not implemented");
    }

    fn literal(&mut self) {
        match self.tokens[self.current - 1].token_type {
            TokenType::Null => self.emit_byte(OpCode::Null as u8),
            TokenType::True => self.emit_byte(OpCode::True as u8),
            TokenType::False => self.emit_byte(OpCode::False as u8),
            _ => self.error_at_current("Expected literal (Null, True, False)"),
        }
    }

    fn unary(&mut self) {
        let operator = self.tokens[self.current - 1].token_type;
        self.parse_precedence(Precedence::Unary);
        match operator {
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8),
            TokenType::Bang => self.emit_byte(OpCode::Not as u8),
            _ => self.error_at_current("Expected unary operator"),
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let index: u8 = self.chunk.constants.len() as u8;
        self.emit_bytes(OpCode::Constant as u8, index);
        self.chunk.constants.push(value);
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

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.code.push(byte);
        self.chunk
            .lines
            .push(self.tokens[self.current - 1].line as usize);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn binary(&mut self) {
        let operator = self.tokens[self.current - 1].token_type;
        let rule: ParseRule = self.rules[&operator];
        self.parse_precedence(rule.precedence.next());
        match operator {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Minus => self.emit_byte(OpCode::Sub as u8),
            TokenType::Star => self.emit_byte(OpCode::Mul as u8),
            TokenType::Slash => self.emit_byte(OpCode::Div as u8),
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
        self.emit_byte(OpCode::Print as u8);
    }

    fn for_statement(&mut self) {
        panic!("For statement not implemented");
    }

    fn if_statement(&mut self) {
        panic!("If statement not implemented");
    }

    fn return_statement(&mut self) {
        panic!("Return statement not implemented");
    }

    fn while_statement(&mut self) {
        panic!("While statement not implemented");
    }

    fn begin_scope(&mut self) {
        panic!("Begin scope not implemented");
    }

    fn end_scope(&mut self) {
        panic!("End scope not implemented");
    }

    fn block(&mut self) {
        panic!("Block not implemented");
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after expression");
        self.emit_byte(OpCode::Pop as u8);
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
        self.chunk.constants.push(value.clone());
        self.chunk.constants.len() as u8 - 1
    }

    // TODO: Disallow redefining globals
    fn global_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expected variable name");
        let global = self.identifier_constant();
        self.emit_bytes(OpCode::DefineGlobal as u8, global);
        if self.match_(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Null as u8);
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
        } else {
            self.statement();
        }
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
    parser.emit_byte(OpCode::Return as u8);
    if parser.error {
        CompilerResult::CompileError
    } else {
        CompilerResult::Chunk(parser.chunk)
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
            OpCode::Return as u8,
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
            OpCode::Return as u8,
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
            OpCode::Return as u8,
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
            OpCode::Return as u8,
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
            OpCode::Return as u8,
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
            OpCode::Return as u8,
        ];
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }

    #[test]
    fn defining_globals() {
        let chunk: Chunk = compile_to_chunk("global a; global b = 1;");
        let expected = [
            OpCode::DefineGlobal as u8,
            0, // Points to "a"
            OpCode::Null as u8,
            OpCode::DefineGlobal as u8,
            1, // Points to "b"
            OpCode::Constant as u8,
            2, // Points to 1
            OpCode::Return as u8,
        ];
        chunk.disassemble("test");
        match_bytecode(&chunk, &expected);
    }
}
