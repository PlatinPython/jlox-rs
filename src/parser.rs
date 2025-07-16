use crate::ast::{Expr, Literal, Stmt};
use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct Error {
    message: String,
    line: usize,
}

type Result<T, E = Error> = std::result::Result<T, E>;

trait TokenPred {
    fn matches(&self, token: &Token) -> bool;
}

impl TokenPred for TokenType {
    fn matches(&self, token: &Token) -> bool {
        &token.token_type == self
    }
}

impl<F> TokenPred for F
where
    F: Fn(&Token) -> bool,
{
    fn matches(&self, token: &Token) -> bool {
        self(token)
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut stmts = vec![];
        let mut errors = vec![];
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => errors.push(e),
            }
        }
        if errors.is_empty() {
            Ok(stmts)
        } else {
            Err(errors)
        }
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn declaration(&mut self) -> Result<Stmt> {
        let res = if self.match_(&[TokenType::Var]) {
            return self.var_declaration();
        } else {
            self.statement()
        };
        if res.is_err() {
            self.synchronize();
        }
        res
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.match_(&[TokenType::For]) {
            self.for_statement()
        } else if self.match_(&[TokenType::If]) {
            self.if_statement()
        } else if self.match_(&[TokenType::Print]) {
            self.print_statement()
        } else if self.match_(&[TokenType::While]) {
            self.while_statement()
        } else if self.match_(&[TokenType::LeftBrace]) {
            Ok(Stmt::new_block(self.block()?))
        } else {
            self.expression_statement()
        }
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_(&[TokenType::Semicolon]) {
            None
        } else if self.match_(&[TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;
        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::new_block(vec![body, Stmt::new_expression(increment)]);
        }

        body = Stmt::new_while(
            condition.unwrap_or(Expr::Literal(Literal::True)),
            Box::new(body),
        );

        if let Some(initializer) = initializer {
            body = Stmt::new_block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = if self.match_(&[TokenType::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::new_if(condition, Box::new(then_branch), else_branch))
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::new_print(value))
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self.consume_identifier("Expect variable name.")?;

        let initializer = if self.match_(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        Ok(Stmt::new_var(name, initializer))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;
        let body = self.statement()?;

        Ok(Stmt::new_while(condition, Box::new(body)))
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::new_expression(expr))
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = vec![];

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(stmts)
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.or()?;

        if self.match_(&[TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;

            if let Expr::Variable(var) = expr {
                Ok(Expr::new_assign(var.name, Box::new(value)))
            } else {
                Err(Error {
                    message: "Invalid assignment target".to_string(),
                    line: equals.line,
                })
            }
        } else {
            Ok(expr)
        }
    }

    fn left_assoc_binary(
        &mut self,
        operators: &[TokenType],
        operand: fn(&mut Self) -> Result<Expr>,
        expr_type: fn(Box<Expr>, Token, Box<Expr>) -> Expr,
    ) -> Result<Expr> {
        let mut expr = operand(self)?;

        while self.match_(operators) {
            let operator = self.previous();
            let right = operand(self)?;
            expr = expr_type(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr> {
        self.left_assoc_binary(&[TokenType::Or], Self::and, Expr::new_logical)
    }

    fn and(&mut self) -> Result<Expr> {
        self.left_assoc_binary(&[TokenType::And], Self::equality, Expr::new_logical)
    }

    fn equality(&mut self) -> Result<Expr> {
        self.left_assoc_binary(
            &[TokenType::BangEqual, TokenType::EqualEqual],
            Self::comparison,
            Expr::new_binary,
        )
    }

    fn comparison(&mut self) -> Result<Expr> {
        self.left_assoc_binary(
            &[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
            Self::term,
            Expr::new_binary,
        )
    }

    fn term(&mut self) -> Result<Expr> {
        self.left_assoc_binary(
            &[TokenType::Minus, TokenType::Plus],
            Self::factor,
            Expr::new_binary,
        )
    }

    fn factor(&mut self) -> Result<Expr> {
        self.left_assoc_binary(
            &[TokenType::Slash, TokenType::Star],
            Self::unary,
            Expr::new_binary,
        )
    }

    fn unary(&mut self) -> Result<Expr> {
        if self.match_(&[TokenType::Minus, TokenType::Bang]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expr::new_unary(operator, Box::new(right)));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr> {
        self.advance();
        match self.previous().token_type {
            TokenType::False => Ok(Expr::Literal(Literal::False)),
            TokenType::True => Ok(Expr::Literal(Literal::True)),
            TokenType::Nil => Ok(Expr::Literal(Literal::Nil)),
            TokenType::Number(n) => Ok(Expr::Literal(Literal::Number(n))),
            TokenType::String(s) => Ok(Expr::Literal(Literal::String(s))),
            TokenType::Identifier(_) => Ok(Expr::new_variable(self.previous())),
            TokenType::LeftParen => {
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
                Ok(Expr::new_grouping(Box::new(expr)))
            }
            token_type => Err(Error {
                message: format!("Expected expression, found `{token_type}`"),
                line: self.previous().line,
            }),
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

    fn consume(&mut self, pred: impl TokenPred, message: &str) -> Result<Token> {
        if self.check(&pred) {
            Ok(self.advance())
        } else {
            Err(Error {
                message: message.to_string(),
                line: self.peek().line,
            })
        }
    }

    fn consume_identifier(&mut self, message: &str) -> Result<Token> {
        self.consume(
            |t: &Token| matches!(t.token_type, TokenType::Identifier(_)),
            message,
        )
    }

    fn check(&self, pred: &impl TokenPred) -> bool {
        if self.is_at_end() {
            false
        } else {
            pred.matches(&self.peek())
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
