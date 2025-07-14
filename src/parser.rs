use crate::ast::{Expr, Literal, Stmt};
use crate::lox::Lox;
use crate::token::{Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self, lox: &mut Lox) -> Vec<Stmt> {
        let mut stmts = vec![];
        while !self.is_at_end() {
            stmts.push(self.statement(lox));
        }
        stmts
    }

    fn expression(&mut self, lox: &mut Lox) -> Expr {
        self.equality(lox)
    }

    fn statement(&mut self, lox: &mut Lox) -> Stmt {
        if self.match_(&[TokenType::Print]) {
            return self.print_statement(lox);
        }
        self.expression_statement(lox)
    }

    fn print_statement(&mut self, lox: &mut Lox) -> Stmt {
        let value = self.expression(lox);
        self.consume(lox, TokenType::Semicolon, "Expect ';' after value.");
        Stmt::new_print(value)
    }

    fn expression_statement(&mut self, lox: &mut Lox) -> Stmt {
        let expr = self.expression(lox);
        self.consume(lox, TokenType::Semicolon, "Expect ';' after expression.");
        Stmt::new_expression(expr)
    }

    fn left_assoc_binary(
        &mut self,
        lox: &mut Lox,
        operators: &[TokenType],
        operand: fn(&mut Self, &mut Lox) -> Expr,
    ) -> Expr {
        let mut expr = operand(self, lox);

        while self.match_(operators) {
            let operator = self.previous();
            let right = operand(self, lox);
            expr = Expr::new_binary(Box::new(expr), operator, Box::new(right));
        }

        expr
    }

    fn equality(&mut self, lox: &mut Lox) -> Expr {
        self.left_assoc_binary(
            lox,
            &[TokenType::BangEqual, TokenType::EqualEqual],
            Self::comparison,
        )
    }

    fn comparison(&mut self, lox: &mut Lox) -> Expr {
        self.left_assoc_binary(
            lox,
            &[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
            Self::term,
        )
    }

    fn term(&mut self, lox: &mut Lox) -> Expr {
        self.left_assoc_binary(lox, &[TokenType::Minus, TokenType::Plus], Self::factor)
    }

    fn factor(&mut self, lox: &mut Lox) -> Expr {
        self.left_assoc_binary(lox, &[TokenType::Slash, TokenType::Star], Self::unary)
    }

    fn unary(&mut self, lox: &mut Lox) -> Expr {
        if self.match_(&[TokenType::Minus, TokenType::Bang]) {
            let operator = self.previous();
            let right = self.unary(lox);
            return Expr::new_unary(operator, Box::new(right));
        }

        self.primary(lox)
    }

    fn primary(&mut self, lox: &mut Lox) -> Expr {
        self.advance();
        match self.previous().token_type {
            TokenType::False => Expr::Literal(Literal::False),
            TokenType::True => Expr::Literal(Literal::True),
            TokenType::Nil => Expr::Literal(Literal::Nil),
            TokenType::Number(n) => Expr::Literal(Literal::Number(n)),
            TokenType::String(s) => Expr::Literal(Literal::String(s)),
            TokenType::LeftParen => {
                let expr = self.expression(lox);
                self.consume(lox, TokenType::RightParen, "Expect ')' after expression.");
                Expr::new_grouping(Box::new(expr))
            }
            _ => self.error(lox, self.peek(), "Expected expression."),
        }
    }

    fn match_(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn consume(&mut self, lox: &mut Lox, token_type: TokenType, message: &str) -> Token {
        if self.check(&token_type) {
            return self.advance();
        }
        self.error(lox, self.peek(), message);
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().token_type == *token_type
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn error(&self, lox: &mut Lox, token: Token, message: &str) -> ! {
        lox.token_error(token, message);
        panic!()
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return;
                }
                _ => {}
            }
            self.advance();
        }
    }
}
