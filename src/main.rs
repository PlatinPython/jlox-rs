use std::{env, process};

use crate::expr::{AstPrinter, Expr, Literal};
use crate::lox::Lox;
use crate::token::{Token, TokenType};

mod expr;
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

    let expr = Expr::new_binary(
        Box::new(Expr::new_unary(
            Token::new(TokenType::Minus, "-".to_string(), 1),
            Box::new(Expr::Literal(Literal::Number(123f64))),
        )),
        Token::new(TokenType::Star, "*".to_string(), 1),
        Box::new(Expr::new_grouping(Box::new(Expr::Literal(
            Literal::Number(45.67),
        )))),
    );

    println!("{}", AstPrinter.print(&expr));
}
