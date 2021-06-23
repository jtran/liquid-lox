use fnv::FnvHashMap;

use crate::value::*;

#[derive(Clone, Debug, PartialEq)]
pub struct FieldTable {
    map: FnvHashMap<String, Value>,
}

impl Default for FieldTable {
    fn default() -> FieldTable {
        FieldTable {
            map: FnvHashMap::default(),
        }
    }
}

impl FieldTable {
    pub fn new() -> FieldTable {
        Default::default()
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.map.get(name).cloned()
    }

    // Returns true if a new entry was added.
    pub fn set(&mut self, name: &str, new_value: Value) -> bool {
        self.map.insert(name.to_string(), new_value).is_none()
    }

    pub fn delete(&mut self, name: &str) {
        self.map.remove(name);
    }
}
