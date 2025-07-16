use std::collections::HashMap;

use crate::interpreter::Value;
use crate::token::Token;

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_enclosing(enclosing: Environment) -> Environment {
        Self {
            values: HashMap::new(),
            enclosing: Some(Box::new(enclosing)),
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
                .and_then(|enclosing| enclosing.get(name)),
        }
    }

    pub fn assign(&mut self, name: &Token, value: Value) -> bool {
        if !self.values.contains_key(&name.lexeme) {
            return self
                .enclosing
                .as_mut()
                .is_some_and(|enclosing| enclosing.assign(name, value));
        }

        self.values.insert(name.lexeme.clone(), value);
        true
    }
}
