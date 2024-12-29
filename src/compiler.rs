use crate::{chunk::*, scanner::*, vm::Value};

struct Parser {
    tokens: Vec<Token>,
    current: usize,
    error: bool,
    chunk: Chunk,
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
        self.error = true;
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
            _ => (),
        }
    }

    pub fn expression(&mut self) {
        self.number();
        self.binary();
    }
}

pub fn compile(tokens: &[Token]) -> Chunk {
    let chunk = Chunk::new();
    let mut parser = Parser {
        tokens: tokens.to_vec(),
        current: 0,
        error: false,
        chunk,
    };
    while !parser.match_(TokenType::Eof) {
        parser.expression();
    }
    parser.emit_byte(OpCode::Return as u8);
    parser.chunk
}

pub fn compile_full(tokens: &str) -> Chunk {
    let tokens: Vec<Token> = scan(tokens);
    compile(&tokens)
}

#[cfg(test)]
mod tests {
    use crate::compiler::*;

    #[test]
    fn basic_expression() {
        let chunk: Chunk = compile_full("1+1");
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
        let chunk: Chunk = compile_full("1+1-1/1*1");
        chunk.disassemble("test");
    }

    #[test]
    fn full_compile() {
        let chunk = compile_full("1 + 2 - 3;");
        chunk.disassemble("test");
        let expected = [
            OpCode::Constant as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Add as u8,
            OpCode::Constant as u8,
            2,
            OpCode::Sub as u8,
            OpCode::Return as u8,
        ];
        assert_eq!(expected.len(), chunk.code.len());
        for (i, byte) in expected.iter().enumerate() {
            assert_eq!(*byte, chunk.code[i]);
        }
    }
}
