use std::fs::File;
use std::io::Read;
use std::{io, process};

use crate::expr::AstPrinter;
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};

pub struct Lox {
    had_error: bool,
}

impl Lox {
    pub fn new() -> Self {
        Self { had_error: false }
    }

    pub fn run_file(&mut self, path: &str) {
        let mut buffer = String::new();
        File::open(path)
            .unwrap()
            .read_to_string(&mut buffer)
            .unwrap();
        self.run(buffer);

        if self.had_error {
            process::exit(65);
        }
    }

    pub fn run_prompt(&mut self) {
        loop {
            print!("> ");
            let mut line = String::new();
            if io::stdin().read_line(&mut line).unwrap() == 0 {
                break;
            }
            self.run(line);
            self.had_error = false;
        }
    }

    fn run(&mut self, source: String) {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens(self);
        let mut parser = Parser::new(tokens);
        let expr = parser.parse(self);

        if self.had_error {
            return;
        }

        println!("{}", AstPrinter.print(&expr));
    }

    pub fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }

    fn report(&mut self, line: usize, source: &str, message: &str) {
        eprintln!("[line {line}] Error{source}: {message}");
        self.had_error = true;
    }

    pub fn token_error(&mut self, token: Token, message: &str) {
        if token.token_type == TokenType::Eof {
            self.report(token.line, " at end", message);
        } else {
            self.report(token.line, &format!(" at '{}'", token.lexeme), message);
        }
    }
}
