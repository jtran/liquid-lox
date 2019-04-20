use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::ops::Deref;
use std::rc::Rc;

use crate::value::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    values: HashMap<String, Value>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
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
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Environment {
        Environment {
            enclosing,
            ..Default::default()
        }
    }

    pub fn define(&mut self, key: &str, value: Value) {
        self.values.insert(key.to_string(), value);
    }

    pub fn get_at(&self, key: &str, distance: usize) -> Option<Value> {
        if distance == 0 {
            // TODO: Don't clone here since it copies strings.  We only have a
            // reference to the value, though.  We cannot change the return type
            // to be a reference because that conflicts with getting enclosing
            // environments.
            return self.values.get(key).cloned();
        }

        // TODO: implement this without recursion.
        match self.enclosing {
            None => panic!("tried to look up a variable at a distance greater than exists: {}", key),
            Some(ref env_cell_rc) => {
                let env = env_cell_rc.deref().borrow();

                env.get_at(key, distance - 1)
            }
        }
    }

    // Returns an error result if the key isn't already defined.
    pub fn assign_at(&mut self, key: &str, distance: usize, new_value: Value)
        -> Result<(), ()>
    {
        if distance == 0 {
            // Assign at this level.
            let entry = self.values.entry(key.to_string())
                            .and_modify(|old_value| { old_value.clone_from(&new_value); });
            return match entry {
                Entry::Occupied(_) => Ok(()),
                Entry::Vacant(_) => Err(()), // Not found.
            }
        }

        // TODO: implement this without recursion.
        match self.enclosing {
            None => panic!("tried to look up a variable at a distance greater than exists: {}", key),
            Some(ref env_cell_rc) => {
                let mut env = env_cell_rc.deref().borrow_mut();

                env.assign_at(key, distance - 1, new_value)
            }
        }
    }
}
