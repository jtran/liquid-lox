use std::collections::HashMap;
use std::collections::hash_map::Entry;

use value::*;

#[derive(Clone, Debug)]
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

    pub fn get_at(&self, key: &str, distance: usize) -> Option<&Value> {
        match self.ancestor(distance) {
            None => panic!("tried to look up a variable at a distance greater than exists: {} distance={}", key, distance),
            Some(env) => env.values.get(key),
        }
    }

    // Returns an error result if the key isn't already defined.
    pub fn assign_at(&mut self, key: &str, distance: usize, new_value: Value) -> Result<(), ()> {
        match self.ancestor_mut(distance) {
            None => panic!("tried to look up a variable at a distance greater than exists: {} distance={}", key, distance),
            Some(ref mut env) => {
                // Assign at this level.
                let entry = env.values.entry(key.to_string())
                                .and_modify(|old_value| { old_value.clone_from(&new_value); });
                return match entry {
                    Entry::Occupied(_) => Ok(()),
                    Entry::Vacant(_) => Err(()), // Not found.
                }
            }
        };
    }

    fn ancestor(&self, distance: usize) -> Option<&Environment> {
        // TODO: implement this without recursion.
        if distance == 0 {
            Some(self)
        }
        else {
            match self.enclosing {
                Some(ref env) => env.ancestor(distance - 1),
                None => None,
            }
        }
    }

    fn ancestor_mut(&mut self, distance: usize) -> Option<&mut Environment> {
        // TODO: implement this without recursion.
        if distance == 0 {
            Some(self)
        }
        else {
            match self.enclosing {
                Some(ref mut env) => env.ancestor_mut(distance - 1),
                None => None,
            }
        }
    }
}
