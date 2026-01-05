use std::cell::RefCell;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::time::SystemTime;

use crate::ast;
use crate::ast::{
    Assign, Binary, Block, Call, Expr, ExprVisitor, Expression, Get, Grouping, If, Literal,
    Logical, Print, Return, Set, Stmt, StmtVisitor, This, Unary, Var, Variable, Walkable, While,
};
use crate::environment::{Environment, EnvironmentExt};
use crate::token::{Token, TokenType};

trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result;
}

#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    pub fun: fn(&mut Interpreter, Vec<Value>) -> Result,
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Callable for NativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result {
        (self.fun)(interpreter, arguments)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub declaration: ast::Function,
    pub closure: Rc<RefCell<Environment>>,
    pub is_initializer: bool,
}

impl Function {
    fn bind(&self, instance: Rc<RefCell<Instance>>) -> Self {
        let mut environment = Environment::new_enclosing(self.closure.clone());
        environment.define("this", Value::Instance(instance));
        Self {
            declaration: self.declaration.clone(),
            closure: Rc::new(RefCell::new(environment)),
            is_initializer: self.is_initializer,
        }
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result {
        let mut environment = Environment::new_enclosing(self.closure.clone());
        for (param, arg) in self.declaration.params.iter().zip(arguments) {
            environment.define(&param.lexeme, arg.clone());
        }

        match interpreter.execute_block(&self.declaration.body, Rc::new(RefCell::new(environment)))
        {
            Ok(_) | Err(Error::Return { .. }) if self.is_initializer => Ok(self
                .closure
                .get_at(0, "this")
                .expect("The current environment should exist.")),
            Err(Error::Return { value }) => Ok(*value),
            Err(err) => Err(err),
            _ => Ok(Value::Nil),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Function>,
}

impl Class {
    pub fn find_method(&self, name: &str) -> Option<&Function> {
        self.methods.get(name)
    }
}

impl Callable for Rc<Class> {
    fn arity(&self) -> usize {
        if let Some(initializer) = self.find_method("init") {
            initializer.arity()
        } else {
            0
        }
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result {
        let instance = Rc::new(RefCell::new(Instance {
            class: self.clone(),
            fields: HashMap::new(),
        }));
        if let Some(initializer) = self.find_method("init") {
            initializer
                .bind(instance.clone())
                .call(interpreter, arguments)?;
        }
        Ok(Value::Instance(instance))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    pub class: Rc<Class>,
    pub fields: HashMap<String, Value>,
}

trait ImplExt {
    fn get(&self, name: &Token) -> Result;
    fn set(&self, name: &Token, value: Value) -> Result;
}

impl ImplExt for Rc<RefCell<Instance>> {
    fn get(&self, name: &Token) -> Result {
        if let Some(val) = self.borrow().fields.get(&name.lexeme) {
            Ok(val.clone())
        } else if let Some(method) = self.borrow().class.find_method(&name.lexeme) {
            Ok(Value::Function(method.bind(self.clone())))
        } else {
            Err(Error::Error {
                token: name.clone(),
                message: format!("Undefined property '{}'.", name.lexeme),
            })
        }
    }

    fn set(&self, name: &Token, value: Value) -> Result {
        self.borrow_mut()
            .fields
            .insert(name.lexeme.clone(), value.clone());
        Ok(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    NativeFunction(NativeFunction),
    Function(Function),
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
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
            Value::NativeFunction(n) => write!(f, "<native fn {}>", n.name),
            Value::Function(Function { declaration, .. }) => {
                write!(f, "<fn {}>", declaration.name.lexeme)
            }
            Value::Class(class) => write!(f, "{}", class.name),
            Value::Instance(instance) => write!(f, "{} instance", instance.borrow().class.name),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    #[allow(dead_code)]
    Error {
        token: Token,
        message: String,
    },
    Return {
        value: Box<Value>,
    },
}

type Result<T = Value> = std::result::Result<T, Error>;

pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>,
    locals: HashMap<Expr, usize>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let globals = Rc::new(RefCell::new(Environment::new()));
        globals.borrow_mut().define(
            "clock",
            Value::NativeFunction(NativeFunction {
                name: "clock".to_string(),
                arity: 0,
                fun: |_, _| {
                    Ok(Value::Number(
                        SystemTime::now()
                            .duration_since(SystemTime::UNIX_EPOCH)
                            .expect("System time is before unix epoch")
                            .as_secs_f64(),
                    ))
                },
            }),
        );
        Self {
            globals: globals.clone(),
            environment: globals.clone(),
            locals: HashMap::new(),
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

    fn execute_block(
        &mut self,
        stmts: &[Stmt],
        environment: Rc<RefCell<Environment>>,
    ) -> Result<()> {
        let previous = self.environment.clone();
        self.environment = environment;
        let result = stmts.iter().try_for_each(|stmt| self.execute(stmt));
        self.environment = previous;
        result
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) {
        self.locals.insert(expr.clone(), depth);
    }

    fn look_up_variable(&self, name: &Token, expr: &Expr) -> Result {
        match self.locals.get(expr) {
            Some(distance) => self.environment.get_at(*distance, &name.lexeme),
            None => self.globals.borrow().get(name),
        }
        .ok_or(Error::Error {
            token: name.clone(),
            message: format!("Undefined variable '{}'.", name.lexeme),
        })
    }

    fn error(&self, token: &Token, message: &str) -> Result {
        Err(Error::Error {
            token: token.clone(),
            message: message.to_string(),
        })
    }
}

impl ExprVisitor<Result> for &mut Interpreter {
    fn visit_assign(self, expr: &Assign) -> Result {
        let value = self.evaluate(&expr.value)?;
        match self.locals.get(&Expr::Assign(expr.clone())) {
            Some(distance) => self.environment.assign_at(*distance, &expr.name, value),
            None => self.globals.borrow_mut().assign(&expr.name, value),
        }
        .ok_or(Error::Error {
            token: expr.name.clone(),
            message: format!("Undefined variable '{}'.", expr.name.lexeme),
        })
    }

    //noinspection DuplicatedCode
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

    fn visit_call(self, expr: &Call) -> Result {
        let callee = self.evaluate(&expr.callee)?;

        let mut arguments = vec![];
        for argument in &expr.arguments {
            arguments.push(self.evaluate(argument)?);
        }

        let function: Box<dyn Callable> = match callee {
            Value::NativeFunction(n) => Box::new(n),
            Value::Function(f) => Box::new(f),
            Value::Class(c) => Box::new(c),
            _ => return self.error(&expr.paren, "Can only call functions and classes."),
        };

        if arguments.len() != function.arity() {
            return self.error(
                &expr.paren,
                &format!(
                    "Expected {} arguments but got {}.",
                    function.arity(),
                    arguments.len()
                ),
            );
        }

        function.call(self, arguments)
    }

    fn visit_get(self, expr: &Get) -> Result {
        if let Value::Instance(object) = self.evaluate(&expr.object)? {
            object.get(&expr.name)
        } else {
            self.error(&expr.name, "Only instances have properties.")
        }
    }

    fn visit_grouping(self, expr: &Grouping) -> Result {
        self.evaluate(&expr.expr)
    }

    fn visit_literal(self, expr: &Literal) -> Result {
        match expr {
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Number(n) => Ok(Value::Number(n.0)),
            Literal::True => Ok(Value::Bool(true)),
            Literal::False => Ok(Value::Bool(false)),
            Literal::Nil => Ok(Value::Nil),
        }
    }

    fn visit_logical(self, expr: &Logical) -> Result {
        let left = self.evaluate(&expr.left)?;

        if expr.operator.token_type == TokenType::Or {
            if left.is_truthy() {
                return Ok(left);
            }
        } else if !left.is_truthy() {
            return Ok(left);
        }

        self.evaluate(&expr.right)
    }

    fn visit_set(self, expr: &Set) -> Result {
        if let Value::Instance(object) = self.evaluate(&expr.object)? {
            let value = self.evaluate(&expr.value)?;
            object.set(&expr.name, value)
        } else {
            self.error(&expr.name, "Only instances have fields.")
        }
    }

    fn visit_this(self, expr: &This) -> Result {
        self.look_up_variable(&expr.keyword, &Expr::This(expr.clone()))
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
        self.look_up_variable(&expr.name, &Expr::Variable(expr.clone()))
    }
}

impl StmtVisitor<Result<()>> for &mut Interpreter {
    fn visit_block(self, stmt: &Block) -> Result<()> {
        self.execute_block(
            &stmt.stmts,
            Rc::new(RefCell::new(Environment::new_enclosing(
                self.environment.clone(),
            ))),
        )
    }

    fn visit_class(self, stmt: &ast::Class) -> Result<()> {
        self.environment
            .borrow_mut()
            .define(&stmt.name.lexeme, Value::Nil);

        let mut methods = HashMap::new();
        for method in &stmt.methods {
            let function = Function {
                declaration: method.clone(),
                closure: self.environment.clone(),
                is_initializer: method.name.lexeme == "init",
            };
            methods.insert(method.name.lexeme.clone(), function);
        }

        let class = Value::Class(Rc::new(Class {
            name: stmt.name.lexeme.clone(),
            methods,
        }));
        self.environment.borrow_mut().assign(&stmt.name, class);

        Ok(())
    }

    fn visit_expression(self, stmt: &Expression) -> Result<()> {
        self.evaluate(&stmt.expr).map(|_| ())
    }

    fn visit_function(self, stmt: &ast::Function) -> Result<()> {
        let function = Value::Function(Function {
            declaration: stmt.clone(),
            closure: self.environment.clone(),
            is_initializer: false,
        });
        self.environment
            .borrow_mut()
            .define(&stmt.name.lexeme, function);
        Ok(())
    }

    fn visit_if(self, stmt: &If) -> Result<()> {
        if self.evaluate(&stmt.condition)?.is_truthy() {
            self.execute(&stmt.then_branch)?;
        } else if let Some(else_branch) = &stmt.else_branch {
            self.execute(else_branch)?;
        }
        Ok(())
    }

    fn visit_print(self, stmt: &Print) -> Result<()> {
        let value = self.evaluate(&stmt.expr)?;
        println!("{value}");
        Ok(())
    }

    fn visit_return(self, stmt: &Return) -> Result<()> {
        let value = if let Some(value) = &stmt.value {
            self.evaluate(value)?
        } else {
            Value::Nil
        };

        Err(Error::Return {
            value: Box::new(value),
        })
    }

    fn visit_var(self, stmt: &Var) -> Result<()> {
        let value = match &stmt.initializer {
            Some(expr) => self.evaluate(expr)?,
            None => Value::Nil,
        };
        self.environment
            .borrow_mut()
            .define(&stmt.name.lexeme, value);
        Ok(())
    }

    fn visit_while(self, stmt: &While) -> Result<()> {
        while self.evaluate(&stmt.condition)?.is_truthy() {
            self.execute(&stmt.body)?;
        }
        Ok(())
    }
}
