use crate::token::{Token, TokenType};

#[allow(dead_code)]
#[derive(Debug)]
pub enum Error {
    UnexpectedCharacter { character: char, line: usize },
    UnterminatedString { line: usize },
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, Vec<Error>> {
        let mut errors = vec![];

        while !self.is_at_end() {
            self.start = self.current;
            match self.scan_token() {
                Ok(Some(token_type)) => self.add_token(token_type),
                Ok(None) => {}
                Err(e) => errors.push(e),
            }
        }

        self.tokens
            .push(Token::new(TokenType::Eof, String::new(), self.line));
        if errors.is_empty() {
            Ok(self.tokens.clone())
        } else {
            Err(errors)
        }
    }

    fn scan_token(&mut self) -> Result<Option<TokenType>> {
        let c = self.advance();
        match c {
            '(' => Ok(Some(TokenType::LeftParen)),
            ')' => Ok(Some(TokenType::RightParen)),
            '{' => Ok(Some(TokenType::LeftBrace)),
            '}' => Ok(Some(TokenType::RightBrace)),
            ',' => Ok(Some(TokenType::Comma)),
            '.' => Ok(Some(TokenType::Dot)),
            '-' => Ok(Some(TokenType::Minus)),
            '+' => Ok(Some(TokenType::Plus)),
            ';' => Ok(Some(TokenType::Semicolon)),
            '*' => Ok(Some(TokenType::Star)),
            '!' => {
                let token_type = if self.consume('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                Ok(Some(token_type))
            }
            '=' => {
                let token_type = if self.consume('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                Ok(Some(token_type))
            }
            '<' => {
                let token_type = if self.consume('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                Ok(Some(token_type))
            }
            '>' => {
                let token_type = if self.consume('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                Ok(Some(token_type))
            }
            '/' => {
                if self.consume('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    Ok(None)
                } else {
                    Ok(Some(TokenType::Slash))
                }
            }
            ' ' | '\r' | '\t' => Ok(None),
            '\n' => {
                self.line += 1;
                Ok(None)
            }
            '"' => self.string().map(Some),
            c if c.is_ascii_digit() => Ok(Some(self.number())),
            c if c.is_ascii_alphabetic() || c == '_' => Ok(Some(self.identifier())),
            c => Err(Error::UnexpectedCharacter {
                character: c,
                line: self.line,
            }),
        }
    }

    fn identifier(&mut self) -> TokenType {
        while {
            let c = self.peek();
            c.is_ascii_alphanumeric() || c == '_'
        } {
            self.advance();
        }

        let text = self.source[self.start..self.current].to_string();
        match text.as_str() {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier(text),
        }
    }

    fn number(&mut self) -> TokenType {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Look for a fractional part.
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // Consume the "."
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let value = self.source[self.start..self.current]
            .to_string()
            .parse()
            .expect("This should be a valid f64.");
        TokenType::Number(value)
    }

    fn string(&mut self) -> Result<TokenType> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(Error::UnterminatedString { line: self.line });
        }

        // The closing ".
        self.advance();

        let value = self.source[self.start + 1..self.current - 1].to_string();
        Ok(TokenType::String(value))
    }

    fn consume(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.as_bytes()[self.current] as char != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source.as_bytes()[self.current] as char
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.source.as_bytes()[self.current + 1] as char
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        let c = self.source.as_bytes()[self.current];
        self.current += 1;
        c as char
    }

    fn add_token(&mut self, token_type: TokenType) {
        let text = self.source[self.start..self.current].to_string();
        self.tokens.push(Token::new(token_type, text, self.line));
    }
}
