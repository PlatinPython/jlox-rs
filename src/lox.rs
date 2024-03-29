use crate::scanner::Scanner;
use std::fs::File;
use std::io::Read;
use std::{io, process};

pub struct Lox {
    had_error: bool,
}

impl Lox {
    pub(crate) fn new() -> Self {
        Self { had_error: false }
    }

    pub(crate) fn run_file(&mut self, path: &str) {
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

    pub(crate) fn run_prompt(&mut self) {
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

        for token in tokens {
            println!("{:?}", token);
        }
    }

    pub(crate) fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }

    fn report(&mut self, line: usize, source: &str, message: &str) {
        eprintln!("[line {}] Error{}: {}", line, source, message);
        self.had_error = true;
    }
}
