use crate::lox::Lox;
use std::{env, process};

mod lox;
mod scanner;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();
    #[allow(clippy::comparison_chain)]
    if args.len() > 2 {
        println!("Usage: {} [script]", args[0]);
        process::exit(64);
    } else if args.len() == 2 {
        Lox::new().run_file(&args[1]);
    } else {
        Lox::new().run_prompt();
    }
}
