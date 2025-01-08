use std::fmt;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
#[allow(dead_code)]
#[repr(u8)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    DotDot,
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
    SlashDown,

    Identifier,
    String,
    Number,
    And,
    Class,
    Else,
    Elseif,
    False,
    Fn,
    For,
    Global,
    If,
    In,
    Null,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Let,
    Const,
    While,

    Error,
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: u32,
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
                '/' => {
                    if self.peek_offset(1) == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else if self.peek_offset(1) == '*' {
                        self.advance();
                        self.advance();
                        while !(self.is_at_end()
                            || self.peek() == '*' && self.peek_offset(1) == '/')
                        {
                            if self.peek() == '\n' {
                                self.line += 1;
                            }
                            self.advance();
                        }
                        self.advance();
                        self.advance();
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
        if self.peek() == '.' && self.peek_offset(1).is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        Token::new(
            TokenType::Number,
            self.source[self.start..self.current].iter().collect(),
            self.line,
        )
    }

    pub fn identifier(&mut self) -> Token {
        while self.peek().is_ascii_alphanumeric() {
            self.advance();
        }
        let lexeme: String = self.source[self.start..self.current].iter().collect();
        // TODO PERF: match on only the first couple chars with a match
        match lexeme.as_str() {
            "and" => Token::new(TokenType::And, lexeme, self.line),
            "class" => Token::new(TokenType::Class, lexeme, self.line),
            "else" => Token::new(TokenType::Else, lexeme, self.line),
            "false" => Token::new(TokenType::False, lexeme, self.line),
            "for" => Token::new(TokenType::For, lexeme, self.line),
            "fn" => Token::new(TokenType::Fn, lexeme, self.line),
            "global" => Token::new(TokenType::Global, lexeme, self.line),
            "if" => {
                if !self.tokens.is_empty()
                    && self.tokens.last().unwrap().token_type == TokenType::Else
                {
                    self.tokens.pop();
                    Token::new(TokenType::Elseif, lexeme, self.line)
                } else {
                    Token::new(TokenType::If, lexeme, self.line)
                }
            }
            "in" => Token::new(TokenType::In, lexeme, self.line),
            "null" => Token::new(TokenType::Null, lexeme, self.line),
            "or" => Token::new(TokenType::Or, lexeme, self.line),
            "print" => Token::new(TokenType::Print, lexeme, self.line),
            "return" => Token::new(TokenType::Return, lexeme, self.line),
            "super" => Token::new(TokenType::Super, lexeme, self.line),
            "this" => Token::new(TokenType::This, lexeme, self.line),
            "true" => Token::new(TokenType::True, lexeme, self.line),
            "let" => Token::new(TokenType::Let, lexeme, self.line),
            "const" => Token::new(TokenType::Const, lexeme, self.line),
            "while" => Token::new(TokenType::While, lexeme, self.line),
            _ => Token::new(TokenType::Identifier, lexeme, self.line),
        }
    }

    pub fn string(&mut self, c: char) -> Token {
        while self.peek() != c {
            if self.peek() == '\n' || self.is_at_end() {
                // Don't allow multiline strings (TODO allow it?)
                return Token::new(TokenType::Error, String::from(""), self.line);
            }
            self.advance();
        }
        self.advance(); // consume closing
        Token::new(
            TokenType::String,
            self.source[self.start + 1..self.current - 1]
                .iter()
                .collect(),
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
        if c.is_ascii_alphabetic() {
            return self.identifier();
        }

        match c {
            '(' => Token::new(TokenType::LeftParen, String::from("("), self.line),
            ')' => Token::new(TokenType::RightParen, String::from(")"), self.line),
            '{' => Token::new(TokenType::LeftBrace, String::from("{"), self.line),
            '}' => Token::new(TokenType::RightBrace, String::from("}"), self.line),
            '[' => Token::new(TokenType::LeftBracket, String::from("["), self.line),
            ']' => Token::new(TokenType::RightBracket, String::from("]"), self.line),
            ',' => Token::new(TokenType::Comma, String::from(","), self.line),
            '-' => Token::new(TokenType::Minus, String::from("-"), self.line),
            '+' => Token::new(TokenType::Plus, String::from("+"), self.line),
            ';' => Token::new(TokenType::Semicolon, String::from(";"), self.line),
            '*' => Token::new(TokenType::Star, String::from("*"), self.line),
            '/' => {
                if self.peek() == '_' {
                    self.advance();
                    Token::new(TokenType::SlashDown, String::from("/_"), self.line)
                } else {
                    Token::new(TokenType::Slash, String::from("/"), self.line)
                }
            }
            '.' => {
                if self.peek() == '.' {
                    self.advance();
                    Token::new(TokenType::DotDot, String::from(".."), self.line)
                } else {
                    Token::new(TokenType::Dot, String::from("."), self.line)
                }
            }
            '!' => {
                if self.peek() == '=' {
                    self.advance();
                    Token::new(TokenType::BangEqual, String::from("!"), self.line)
                } else {
                    Token::new(TokenType::Bang, String::from("!"), self.line)
                }
            }
            '=' => {
                if self.peek() == '=' {
                    self.advance();
                    Token::new(TokenType::EqualEqual, String::from("=="), self.line)
                } else {
                    Token::new(TokenType::Equal, String::from("="), self.line)
                }
            }
            '<' => {
                if self.peek() == '=' {
                    self.advance();
                    Token::new(TokenType::LessEqual, String::from("<="), self.line)
                } else {
                    Token::new(TokenType::Less, String::from("<"), self.line)
                }
            }
            '>' => {
                if self.peek() == '=' {
                    self.advance();
                    Token::new(TokenType::GreaterEqual, String::from(">="), self.line)
                } else {
                    Token::new(TokenType::Greater, String::from(">"), self.line)
                }
            }
            '\'' | '"' => self.string(c),
            _ => Token::new(TokenType::Error, String::from(""), self.line),
        }
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
    use crate::scanner::{Token, TokenType};

    fn verify_output(tokens: Vec<Token>, expected: Vec<TokenType>) {
        assert_eq!(tokens.len(), expected.len());
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token.token_type, expected[i]);
        }
    }

    #[test]
    fn empty_str_should_give_eof() {
        let input = "";
        let tokens = super::scan(input);
        let expected = vec![TokenType::Eof];
        verify_output(tokens, expected);
    }

    #[test]
    fn skips_whitespace() {
        let input = "     \t          \n            \r\n\n";
        let tokens = super::scan(input);
        let expected = vec![TokenType::Eof];
        verify_output(tokens, expected);
    }

    #[test]
    fn skips_comments() {
        let input = "// this is a comment\n";
        let tokens = super::scan(input);
        let expected = vec![TokenType::Eof];
        verify_output(tokens, expected);
    }

    #[test]
    fn skips_multiline_comments() {
        let input = "/* this is a comment\nand this is another comment */";
        let tokens = super::scan(input);
        let expected = vec![TokenType::Eof];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_numbers() {
        let input = "12345";
        let tokens = super::scan(input);
        let expected = vec![TokenType::Number, TokenType::Eof];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_decimal_numbers() {
        let input = "123.45";
        let tokens = super::scan(input);
        let expected = vec![TokenType::Number, TokenType::Eof];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_keywords() {
        let input = "and class else false for fn if null or print return super this true let while";
        let tokens = super::scan(input);

        let expected = vec![
            TokenType::And,
            TokenType::Class,
            TokenType::Else,
            TokenType::False,
            TokenType::For,
            TokenType::Fn,
            TokenType::If,
            TokenType::Null,
            TokenType::Or,
            TokenType::Print,
            TokenType::Return,
            TokenType::Super,
            TokenType::This,
            TokenType::True,
            TokenType::Let,
            TokenType::While,
            TokenType::Eof,
        ];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_one_char_tokens() {
        let input = "(){}[],.-+;*/=!<>";
        let tokens = super::scan(input);
        let expected = vec![
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::LeftBracket,
            TokenType::RightBracket,
            TokenType::Comma,
            TokenType::Dot,
            TokenType::Minus,
            TokenType::Plus,
            TokenType::Semicolon,
            TokenType::Star,
            TokenType::Slash,
            TokenType::Equal,
            TokenType::Bang,
            TokenType::Less,
            TokenType::Greater,
            TokenType::Eof,
        ];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_two_char_tokens() {
        let input = "!= == <= >=";
        let tokens = super::scan(input);
        let expected = vec![
            TokenType::BangEqual,
            TokenType::EqualEqual,
            TokenType::LessEqual,
            TokenType::GreaterEqual,
            TokenType::Eof,
        ];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_strings() {
        let input = "\"hello world\"";
        let tokens = super::scan(input);
        let expected = vec![TokenType::String, TokenType::Eof];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_global_decls() {
        let input = "global x = 5;";
        let tokens = super::scan(input);
        let expected = vec![
            TokenType::Global,
            TokenType::Identifier,
            TokenType::Equal,
            TokenType::Number,
            TokenType::Semicolon,
            TokenType::Eof,
        ];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_else_ifs() {
        let input = "else if";
        let tokens = super::scan(input);
        let expected = vec![TokenType::Elseif, TokenType::Eof];
        verify_output(tokens, expected);

        let input = "if true {} else if false {} else {}";
        let tokens = super::scan(input);
        let expected = vec![
            TokenType::If,
            TokenType::True,
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::Elseif,
            TokenType::False,
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::Else,
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::Eof,
        ];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_comparison_ops() {
        let input = "<= >= == != < >";
        let tokens = super::scan(input);
        let expected = vec![
            TokenType::LessEqual,
            TokenType::GreaterEqual,
            TokenType::EqualEqual,
            TokenType::BangEqual,
            TokenType::Less,
            TokenType::Greater,
            TokenType::Eof,
        ];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_dotdots() {
        let input = ".. . 5..10";
        let tokens: Vec<Token> = super::scan(input);
        let expected = vec![
            TokenType::DotDot,
            TokenType::Dot,
            TokenType::Number,
            TokenType::DotDot,
            TokenType::Number,
            TokenType::Eof,
        ];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_in() {
        let input = "for i in 0..10";
        let tokens = super::scan(input);
        let expected = vec![
            TokenType::For,
            TokenType::Identifier,
            TokenType::In,
            TokenType::Number,
            TokenType::DotDot,
            TokenType::Number,
            TokenType::Eof,
        ];
        verify_output(tokens, expected);
    }

    #[test]
    fn parses_slashdown() {
        let input = "/_ / a";
        let tokens = super::scan(input);
        let expected = vec![
            TokenType::SlashDown,
            TokenType::Slash,
            TokenType::Identifier,
            TokenType::Eof,
        ];
        verify_output(tokens, expected);
    }
}
