use std::fs::File;
use std::io;
use std::io::{Read, Write};

use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::resolver::Resolver;
use crate::scanner::Scanner;
use crate::{interpreter, parser, resolver, scanner};

#[derive(Debug)]
pub enum Error {
    Scanner(Vec<scanner::Error>),
    Parser(Vec<parser::Error>),
    Resolver(Vec<resolver::Error>),
    Runtime(interpreter::Error),
    Io(io::Error),
}

type Result<T = (), E = Error> = std::result::Result<T, E>;

pub struct Lox;

impl Lox {
    pub fn new() -> Self {
        Self
    }

    pub fn run_file(&mut self, path: &str) -> Result {
        let mut buffer = String::new();
        File::open(path)
            .map_err(Error::Io)?
            .read_to_string(&mut buffer)
            .map_err(Error::Io)?;
        self.run(&mut Interpreter::new(), buffer)
    }

    pub fn run_prompt(&mut self) -> Result {
        let mut interpreter = Interpreter::new();
        loop {
            print!("> ");
            io::stdout().flush().map_err(Error::Io)?;
            let mut line = String::new();
            if io::stdin().read_line(&mut line).map_err(Error::Io)? > 0 {
                match self.run(&mut interpreter, line) {
                    Ok(_) => {}
                    Err(err) => eprintln!("{err:?}"),
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    fn run(&mut self, interpreter: &mut Interpreter, source: String) -> Result {
        let tokens = Scanner::new(source).scan_tokens().map_err(Error::Scanner)?;
        let stmts = Parser::new(tokens).parse().map_err(Error::Parser)?;

        Resolver::new(interpreter)
            .resolve(&stmts)
            .map_err(Error::Resolver)?;

        interpreter.interpret(stmts).map_err(Error::Runtime)
    }
}
