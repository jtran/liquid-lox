use std::collections::HashMap;
use std::collections::hash_map::Entry;

use value::*;

pub struct Environment {
    values: HashMap<String, Value>,
    pub enclosing: Option<Box<Environment>>,
}

impl Default for Environment {
    fn default() -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }
}

impl Environment {
    pub fn new(enclosing: Option<Box<Environment>>) -> Environment {
        Environment {
            enclosing,
            ..Default::default()
        }
    }

    pub fn define(&mut self, key: &str, value: Value) {
        self.values.insert(key.to_string(), value);
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.values.get(key)
            .or_else(||
                // If it's not found, look in the enclosing environment.
                match self.enclosing {
                    None => None,
                    Some(ref env) => env.get(key),
                })
    }

    // Returns an error result if the key isn't already defined.
    pub fn assign(&mut self, key: &str, new_value: Value) -> Result<(),()> {
        let entry = self.values.entry(key.to_string())
                        .and_modify(|old_value| { old_value.clone_from(&new_value); });
        match entry {
            Entry::Occupied(_) => Ok(()),
            Entry::Vacant(_) => {
                // Not found.  Assign in enclosing scope.
                match self.enclosing {
                    None => Err(()),
                    Some(ref mut env) => env.assign(key, new_value),
                }
            }
        }
    }
}
