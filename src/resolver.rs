use std::collections::HashMap;

use crate::ast::{
    Assign, Binary, Block, Call, Expr, ExprVisitor, Expression, Function, Grouping, If, Literal,
    Logical, Print, Return, Stmt, StmtVisitor, Unary, Var, Variable, Walkable, While,
};
use crate::interpreter::Interpreter;
use crate::token::Token;

#[derive(Debug)]
pub struct Error {
    token: Token,
    message: String,
}

type Result = std::result::Result<(), Vec<Error>>;

fn combine_results(left: Result, right: Result) -> Result {
    match (left, right) {
        (Ok(_), Ok(_)) => Ok(()),
        (Err(e), Ok(_)) | (Ok(_), Err(e)) => Err(e),
        (Err(mut left), Err(right)) => {
            left.extend(right);
            Err(left)
        }
    }
}

macro_rules! combine_results {
    ($res:ident, $expr:expr) => {
        $res = combine_results($res, $expr)
    };
}

#[derive(Debug, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
}

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
        }
    }

    pub fn resolve(&mut self, stmts: &[Stmt]) -> Result {
        stmts
            .iter()
            .map(|s| self.resolve_stmt(s))
            .fold(Ok(()), combine_results)
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result {
        stmt.walk(self)
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result {
        expr.walk(self)
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> Result {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return self.error(name, "Already a variable with this name in this scope.");
            }

            scope.insert(name.lexeme.to_string(), false);
        }
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.to_string(), true);
        }
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        if let Some((i, _)) = self
            .scopes
            .iter()
            .enumerate()
            .rfind(|(_, scope)| scope.contains_key(&name.lexeme))
        {
            self.interpreter.resolve(expr, self.scopes.len() - 1 - i);
        }
    }

    fn resolve_function(&mut self, func: &Function, function_type: FunctionType) -> Result {
        let mut res = Ok(());
        let enclosing_function = std::mem::replace(&mut self.current_function, function_type);

        self.begin_scope();
        for param in &func.params {
            combine_results!(res, self.declare(param));
            self.define(param);
        }
        combine_results!(res, self.resolve(&func.body));
        self.end_scope();

        self.current_function = enclosing_function;
        res
    }

    fn error(&self, token: &Token, message: &str) -> Result {
        Err(vec![Error {
            token: token.clone(),
            message: message.to_string(),
        }])
    }
}

impl ExprVisitor<Result> for &mut Resolver<'_> {
    fn visit_assign(self, expr: &Assign) -> Result {
        self.resolve_expr(&expr.value)?;
        self.resolve_local(&Expr::Assign(expr.clone()), &expr.name);
        Ok(())
    }

    fn visit_binary(self, expr: &Binary) -> Result {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)?;
        Ok(())
    }

    fn visit_call(self, expr: &Call) -> Result {
        self.resolve_expr(&expr.callee)?;
        for arg in &expr.arguments {
            self.resolve_expr(arg)?;
        }
        Ok(())
    }

    fn visit_grouping(self, expr: &Grouping) -> Result {
        self.resolve_expr(&expr.expr)?;
        Ok(())
    }

    fn visit_literal(self, _: &Literal) -> Result {
        Ok(())
    }

    fn visit_logical(self, expr: &Logical) -> Result {
        let mut res = Ok(());
        combine_results!(res, self.resolve_expr(&expr.left));
        combine_results!(res, self.resolve_expr(&expr.right));
        res
    }

    fn visit_unary(self, expr: &Unary) -> Result {
        self.resolve_expr(&expr.right)?;
        Ok(())
    }

    fn visit_variable(self, expr: &Variable) -> Result {
        if let Some(scope) = self.scopes.last() {
            if let Some(false) = scope.get(&expr.name.lexeme) {
                return self.error(
                    &expr.name,
                    "Can't read local variable in its own initializer.",
                );
            }
        }
        self.resolve_local(&Expr::Variable(expr.clone()), &expr.name);
        Ok(())
    }
}

impl StmtVisitor<Result> for &mut Resolver<'_> {
    fn visit_block(self, stmt: &Block) -> Result {
        self.begin_scope();
        self.resolve(&stmt.stmts)?;
        self.end_scope();
        Ok(())
    }

    fn visit_expression(self, stmt: &Expression) -> Result {
        self.resolve_expr(&stmt.expr)?;
        Ok(())
    }

    fn visit_function(self, stmt: &Function) -> Result {
        let mut res = Ok(());
        combine_results!(res, self.declare(&stmt.name));
        self.define(&stmt.name);
        combine_results!(res, self.resolve_function(stmt, FunctionType::Function));
        res
    }

    fn visit_if(self, stmt: &If) -> Result {
        let mut res = Ok(());
        combine_results!(res, self.resolve_expr(&stmt.condition));
        combine_results!(res, self.resolve_stmt(&stmt.then_branch));
        if let Some(else_branch) = &stmt.else_branch {
            combine_results!(res, self.resolve_stmt(else_branch));
        }
        res
    }

    fn visit_print(self, stmt: &Print) -> Result {
        self.resolve_expr(&stmt.expr)?;
        Ok(())
    }

    fn visit_return(self, stmt: &Return) -> Result {
        if self.current_function == FunctionType::None {
            return self.error(&stmt.keyword, "Can't return from top-level code.");
        }
        if let Some(value) = &stmt.value {
            self.resolve_expr(value)?;
        }
        Ok(())
    }

    fn visit_var(self, stmt: &Var) -> Result {
        let mut res = Ok(());
        combine_results!(res, self.declare(&stmt.name));
        if let Some(initializer) = &stmt.initializer {
            combine_results!(res, self.resolve_expr(initializer));
        }
        self.define(&stmt.name);
        res
    }

    fn visit_while(self, stmt: &While) -> Result {
        let mut res = Ok(());
        combine_results!(res, self.resolve_expr(&stmt.condition));
        combine_results!(res, self.resolve_stmt(&stmt.body));
        res
    }
}
