use std::collections::HashMap;
use std::collections::hash_map::Entry;

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

    // Returns an error result if the key isn't already defined.
    pub fn assign(&mut self, key: &str, new_value: Value) -> Result<(),()> {
        let entry = self.values.entry(key.to_string())
                        .and_modify(|old_value| { old_value.clone_from(&new_value); });
        match entry {
            Entry::Occupied(_) => Ok(()),
            Entry::Vacant(_) => Err(()),
        }
    }
}
