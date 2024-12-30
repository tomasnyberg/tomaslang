use std::collections::HashMap;

use crate::{chunk::*, scanner::*, vm::Value};

struct Parser {
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
    Negate,
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

#[derive(Debug, PartialEq)]
pub enum CompilerResult {
    Chunk(Chunk),
    CompileError,
}

type ParseFn = fn(&mut Parser);

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
            5 => OpCode::Negate,
            6 => OpCode::Return,
            _ => panic!("Unknown opcode"),
        }
    }
}

impl Parser {
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
        rule(LeftParen,    None, None, Precedence::Call);
        rule(RightParen,   None, None, Precedence::None);
        rule(LeftBrace,    None, None, Precedence::None);
        rule(RightBrace,   None, None, Precedence::None);
        rule(Comma,        None, None, Precedence::None);
        rule(Dot,          None, None, Precedence::None);
        rule(Minus,        None, None, Precedence::Term);
        rule(Plus,         None, None, Precedence::Term);
        rule(Semicolon,    None, None, Precedence::None);
        rule(Slash,        None, None, Precedence::Factor);
        rule(Star,         None, None, Precedence::Factor);
        rule(Bang,         None, None, Precedence::None);
        rule(BangEqual,    None, None, Precedence::Equality);
        rule(Equal,        None, None, Precedence::None);
        rule(EqualEqual,   None, None, Precedence::Equality);
        rule(Greater,      None, None, Precedence::Comparison);
        rule(GreaterEqual, None, None, Precedence::Comparison);
        rule(Less,         None, None, Precedence::Comparison);
        rule(LessEqual,    None, None, Precedence::Comparison);
        rule(Identifier,   None, None, Precedence::None);
        rule(String,       None, None, Precedence::None);
        rule(Number,       None, None, Precedence::None);
        rule(And,          None, None, Precedence::And);
        rule(Class,        None, None, Precedence::None);
        rule(Else,         None, None, Precedence::None);
        rule(False,        None, None, Precedence::None);
        rule(Fun,          None, None, Precedence::None);
        rule(For,          None, None, Precedence::None);
        rule(If,           None, None, Precedence::None);
        rule(Null,         None, None, Precedence::None);
        rule(Or,           None, None, Precedence::Or);
        rule(Print,        None, None, Precedence::None);
        rule(Return,       None, None, Precedence::None);
        rule(Super,        None, None, Precedence::None);
        rule(This,         None, None, Precedence::None);
        rule(True,         None, None, Precedence::None);
        rule(Var,          None, None, Precedence::None);
        rule(While,        None, None, Precedence::None);
        rule(Error,        None, None, Precedence::None);
        rule(Eof,          None, None, Precedence::None);

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

    pub fn match_(&mut self, token_type: TokenType) -> bool {
        if self.tokens[self.current].token_type == token_type {
            self.current += 1;
            return true;
        }
        false
    }

    pub fn number(&mut self) {
        if self.match_(TokenType::Number) {
            let number = self.tokens[self.current - 1].lexeme.parse::<f64>().unwrap();
            let index: u8 = self.chunk.constants.len() as u8;
            self.emit_bytes(OpCode::Constant as u8, index);
            self.chunk.constants.push(Value::Number(number));
            return;
        }
        self.error_at_current("Expected number");
    }

    pub fn advance(&mut self) {
        self.current += 1;
    }

    pub fn emit_byte(&mut self, byte: u8) {
        self.chunk.code.push(byte);
        self.chunk
            .lines
            .push(self.tokens[self.current - 1].line as usize);
    }

    pub fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    pub fn binary(&mut self) {
        let operator = self.tokens[self.current].token_type;
        self.advance();
        self.number();
        match operator {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Minus => self.emit_byte(OpCode::Sub as u8),
            TokenType::Star => self.emit_byte(OpCode::Mul as u8),
            TokenType::Slash => self.emit_byte(OpCode::Div as u8),
            _ => self.error_at_current("Expected binary operator"),
        }
    }

    pub fn expression(&mut self) {
        self.number();
        self.binary();
    }
}

pub fn compile(input: &str) -> CompilerResult {
    let mut parser = Parser::new(input);
    while !parser.match_(TokenType::Eof) {
        parser.expression();
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

    #[test]
    fn basic_expression() {
        let compiled = compile("1+1");
        let chunk: Chunk = match compiled {
            CompilerResult::Chunk(chunk) => chunk,
            CompilerResult::CompileError => panic!("Compile error"),
        };
        let expected = [
            OpCode::Constant as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Add as u8,
            OpCode::Return as u8,
        ];
        assert_eq!(expected.len(), chunk.code.len());
        for (i, byte) in expected.iter().enumerate() {
            assert_eq!(*byte, chunk.code[i]);
        }
    }

    #[test]
    fn can_decompile() {
        let compiled = compile("1+1");
        let chunk: Chunk = match compiled {
            CompilerResult::Chunk(chunk) => chunk,
            CompilerResult::CompileError => panic!("Compile error"),
        };
        chunk.disassemble("test");
    }

    #[test]
    fn syntax_error() {
        let compiled = compile("1 +;");
        assert_eq!(compiled, CompilerResult::CompileError);
    }
}
