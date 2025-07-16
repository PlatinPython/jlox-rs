use std::cmp::PartialEq;
use std::fmt::{Display, Formatter};
use std::mem;

use crate::ast::{
    Assign, Binary, Block, Expr, ExprVisitor, Expression, Grouping, Literal, Print, Stmt,
    StmtVisitor, Unary, Var, Variable, Walkable,
};
use crate::environment::Environment;
use crate::token::{Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
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

#[derive(Debug)]
pub struct Error {
    pub token: Token,
    pub message: String,
}

type Result<T = Value> = std::result::Result<T, Error>;

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Self {
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<()> {
        stmts.iter().try_for_each(|stmt| self.execute(stmt))
    }

    fn evaluate(&mut self, expr: &Expr) -> Result {
        expr.walk(self)
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<()> {
        stmt.walk(self)
    }

    fn execute_block(&mut self, stmts: &[Stmt], environment: Environment) -> Result<()> {
        let previous = mem::replace(&mut self.environment, environment);
        let result = stmts.iter().try_for_each(|stmt| self.execute(stmt));
        self.environment = previous;
        result
    }

    fn error(&self, token: &Token, message: &str) -> Result {
        Err(Error {
            token: token.clone(),
            message: message.to_string(),
        })
    }
}

impl ExprVisitor<Result> for &mut Interpreter {
    fn visit_assign(self, expr: &Assign) -> Result {
        let value = self.evaluate(&expr.value)?;
        if !self.environment.assign(&expr.name, value.clone()) {
            self.error(
                &expr.name,
                &format!("Undefined variable '{}'", expr.name.lexeme),
            )
        } else {
            Ok(value)
        }
    }

    fn visit_binary(self, expr: &Binary) -> Result {
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

    fn visit_grouping(self, expr: &Grouping) -> Result {
        self.evaluate(&expr.expr)
    }

    fn visit_literal(self, expr: &Literal) -> Result {
        match expr {
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Number(n) => Ok(Value::Number(*n)),
            Literal::True => Ok(Value::Bool(true)),
            Literal::False => Ok(Value::Bool(false)),
            Literal::Nil => Ok(Value::Nil),
        }
    }

    fn visit_unary(self, expr: &Unary) -> Result {
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

    fn visit_variable(self, expr: &Variable) -> Result {
        self.environment.get(&expr.name).ok_or(Error {
            token: expr.name.clone(),
            message: format!("Undefined variable '{}'.", expr.name.lexeme),
        })
    }
}

impl StmtVisitor<Result<()>> for &mut Interpreter {
    fn visit_block(self, stmt: &Block) -> Result<()> {
        self.execute_block(
            &stmt.stmts,
            Environment::new_enclosing(self.environment.clone()),
        )
    }

    fn visit_expression(self, stmt: &Expression) -> Result<()> {
        self.evaluate(&stmt.expr).map(|_| ())
    }

    fn visit_print(self, stmt: &Print) -> Result<()> {
        let value = self.evaluate(&stmt.expr)?;
        println!("{value}");
        Ok(())
    }

    fn visit_var(self, stmt: &Var) -> Result<()> {
        let value = match &stmt.initializer {
            Some(expr) => self.evaluate(expr)?,
            None => Value::Nil,
        };
        self.environment.define(&stmt.name.lexeme, value);
        Ok(())
    }
}
