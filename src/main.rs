use std::{env, process};

use crate::lox::{Error, Lox};

mod ast;
mod interpreter;
mod lox;
mod parser;
mod scanner;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();
    #[allow(clippy::comparison_chain)]
    if args.len() > 2 {
        println!("Usage: {} [script]", args[0]);
        process::exit(64);
    } else if args.len() == 2 {
        if let Err(err) = Lox::new().run_file(&args[1]) {
            eprintln!("{err:?}");
            process::exit(match err {
                Error::Runtime(_) => 70,
                _ => 65,
            });
        }
    } else {
        Lox::new().run_prompt().expect("REPL error");
    }
}
