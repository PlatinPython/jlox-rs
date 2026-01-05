use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::interpreter::Value;
use crate::token::Token;

#[derive(Debug, PartialEq, Default)]
pub struct Environment {
    values: HashMap<String, Value>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_enclosing(enclosing: Rc<RefCell<Environment>>) -> Environment {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Option<Value> {
        match self.values.get(&name.lexeme) {
            Some(value) => Some(value.clone()),
            None => self
                .enclosing
                .as_ref()
                .and_then(|enclosing| enclosing.borrow().get(name)),
        }
    }

    pub fn assign(&mut self, name: &Token, value: Value) -> Option<Value> {
        if !self.values.contains_key(&name.lexeme) {
            return self
                .enclosing
                .as_mut()
                .and_then(|enclosing| enclosing.borrow_mut().assign(name, value));
        }

        self.values.insert(name.lexeme.clone(), value.clone());
        Some(value)
    }

    fn ancestor(
        environment: Rc<RefCell<Environment>>,
        distance: usize,
    ) -> Option<Rc<RefCell<Environment>>> {
        let mut current = environment;
        for _ in 0..distance {
            let next = current.borrow().enclosing.clone()?;
            current = next;
        }
        Some(current)
    }
}

pub trait EnvironmentExt {
    fn get_at(&self, distance: usize, name: &str) -> Option<Value>;
    fn assign_at(&mut self, distance: usize, name: &Token, value: Value) -> Option<Value>;
}

impl EnvironmentExt for Rc<RefCell<Environment>> {
    fn get_at(&self, distance: usize, name: &str) -> Option<Value> {
        Environment::ancestor(self.clone(), distance)?
            .borrow()
            .values
            .get(name)
            .cloned()
    }

    fn assign_at(&mut self, distance: usize, name: &Token, value: Value) -> Option<Value> {
        Environment::ancestor(self.clone(), distance)?
            .borrow_mut()
            .values
            .insert(name.lexeme.clone(), value.clone());
        Some(value)
    }
}
