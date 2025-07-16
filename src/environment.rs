use std::collections::HashMap;

use crate::interpreter::Value;
use crate::token::Token;

pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Option<Value> {
        self.values.get(&name.lexeme).cloned()
    }

    pub fn assign(&mut self, name: &Token, value: Value) -> bool {
        if !self.values.contains_key(&name.lexeme) {
            return false;
        }

        self.values.insert(name.lexeme.clone(), value);
        true
    }
}
