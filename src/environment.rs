use std::collections::HashMap;

use value::*;

pub struct Environment {
    values: HashMap<String, Value>,
}

impl Default for Environment {
    fn default() -> Environment {
        Environment {
            values: HashMap::new(),
        }
    }
}

impl Environment {
    #[allow(dead_code)]
    pub fn new() -> Environment {
        Environment::default()
    }

    pub fn define(&mut self, key: &str, value: Value) {
        self.values.insert(key.to_string(), value);
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.values.get(key)
    }
}
