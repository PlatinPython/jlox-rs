use std::cmp::PartialEq;
use std::fmt::{Display, Formatter};

use crate::ast::{
    Binary, Expr, ExprVisitor, Expression, Grouping, Literal, Print, Stmt, StmtVisitor, Unary,
    Walkable,
};
use crate::lox::Lox;
use crate::token::{Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => b.fmt(f),
            Value::Number(n) => n.fmt(f),
            Value::String(s) => s.fmt(f),
        }
    }
}

pub struct Error {
    pub token: Token,
    pub message: &'static str,
}

type Result<T> = std::result::Result<T, Error>;

pub struct Interpreter;

impl Interpreter {
    pub fn interpret(&self, lox: &mut Lox, stmts: Vec<Stmt>) {
        if let Err(error) = stmts.iter().try_for_each(|stmt| self.execute(stmt)) {
            lox.runtime_error(error);
        }
    }

    fn evaluate(&self, expr: &Expr) -> Result<Value> {
        expr.walk(self)
    }

    fn execute(&self, stmt: &Stmt) -> Result<()> {
        stmt.walk(self)
    }

    fn error(&self, token: &Token, message: &'static str) -> Result<Value> {
        Err(Error {
            token: token.clone(),
            message,
        })
    }
}

impl ExprVisitor<Result<Value>> for &Interpreter {
    fn visit_binary(self, expr: &Binary) -> Result<Value> {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;

        match expr.operator.token_type {
            TokenType::BangEqual => Ok(Value::Bool(left != right)),
            TokenType::EqualEqual => Ok(Value::Bool(left == right)),
            TokenType::Greater => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l > r)),
                _ => self.error(&expr.operator, "Operands must be numbers."),
            },
            TokenType::GreaterEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l >= r)),
                _ => self.error(&expr.operator, "Operands must be numbers."),
            },
            TokenType::Less => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
                _ => self.error(&expr.operator, "Operands must be numbers."),
            },
            TokenType::LessEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),
                _ => self.error(&expr.operator, "Operands must be numbers."),
            },
            TokenType::Minus => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                _ => self.error(&expr.operator, "Operands must be numbers."),
            },
            TokenType::Plus => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
                _ => self.error(
                    &expr.operator,
                    "Operands must be two numbers or two strings.",
                ),
            },
            TokenType::Slash => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
                _ => self.error(&expr.operator, "Operands must be numbers."),
            },
            TokenType::Star => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                _ => self.error(&expr.operator, "Operands must be numbers."),
            },
            _ => Ok(Value::Nil),
        }
    }

    fn visit_grouping(self, expr: &Grouping) -> Result<Value> {
        self.evaluate(&expr.expr)
    }

    fn visit_literal(self, expr: &Literal) -> Result<Value> {
        match expr {
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Number(n) => Ok(Value::Number(*n)),
            Literal::True => Ok(Value::Bool(true)),
            Literal::False => Ok(Value::Bool(false)),
            Literal::Nil => Ok(Value::Nil),
        }
    }

    fn visit_unary(self, expr: &Unary) -> Result<Value> {
        let right = self.evaluate(&expr.right)?;

        match expr.operator.token_type {
            TokenType::Bang => Ok(Value::Bool(!right.is_truthy())),
            TokenType::Minus => match right {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => self.error(&expr.operator, "Operand must be a number."),
            },
            _ => Ok(Value::Nil),
        }
    }
}

impl StmtVisitor<Result<()>> for &Interpreter {
    fn visit_expression(self, stmt: &Expression) -> Result<()> {
        self.evaluate(&stmt.expr).map(|_| ())
    }

    fn visit_print(self, stmt: &Print) -> Result<()> {
        let value = self.evaluate(&stmt.expr)?;
        println!("{value}");
        Ok(())
    }
}
