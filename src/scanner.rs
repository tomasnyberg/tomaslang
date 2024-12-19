use std::fmt;

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,

    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Null,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    line: u32,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: u32) -> Token {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{:?} {} {}]", self.token_type, self.lexeme, self.line)
    }
}

struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: u32,
}

impl Scanner {
    pub fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    pub fn generate_eof(&mut self) {
        self.tokens
            .push(Token::new(TokenType::Eof, String::from(""), self.line));
    }

    pub fn peek_offset(&self, offset: usize) -> char {
        if self.current + offset >= self.source.len() {
            return '\0';
        }
        self.source[self.current + offset]
    }

    pub fn peek(&self) -> char {
        self.peek_offset(0)
    }

    pub fn skip_whitespace_and_comments(&mut self) {
        loop {
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                // Only allowing single line comments for now
                '/' => {
                    if self.peek_offset(1) == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        break;
                    }
                }
                _ => {
                    break;
                }
            }
        }
    }

    pub fn number(&mut self) -> Token {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        Token::new(
            TokenType::Number,
            self.source[self.start..self.current].iter().collect(),
            self.line,
        )
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();
        self.start = self.current;
        if self.is_at_end() {
            return Token::new(TokenType::Eof, String::from(""), self.line);
        }
        let c = self.advance();
        if c.is_ascii_digit() {
            return self.number();
        }
        Token::new(TokenType::Error, String::from(""), self.line)
    }
}

pub fn scan(input: &str) -> Vec<Token> {
    let tokens: Vec<Token> = Vec::new();
    let char_vector: Vec<char> = input.chars().collect();
    let mut scanner = Scanner {
        source: char_vector,
        tokens,
        start: 0,
        current: 0,
        line: 1,
    };
    loop {
        let token = scanner.scan_token();
        if token.token_type == TokenType::Eof {
            break;
        }
        scanner.tokens.push(token);
    }

    scanner.generate_eof();
    scanner.tokens
}

#[cfg(test)]
mod tests {

    #[test]
    fn empty_str_should_give_eof() {
        let input = "";
        let tokens = super::scan(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, super::TokenType::Eof);
    }

    #[test]
    fn skips_whitespace() {
        let input = "     \t          \n            \r\n\n";
        let tokens = super::scan(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, super::TokenType::Eof);
    }

    #[test]
    fn skips_comments() {
        let input = "// this is a comment\n";
        let tokens = super::scan(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, super::TokenType::Eof);
    }

    #[test]
    fn parses_numbers() {
        let input = "12345";
        let tokens = super::scan(input);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].token_type, super::TokenType::Number);
    }
}
