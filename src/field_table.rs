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

    pub fn set(&mut self, name: &str, new_value: Value) {
        self.map.insert(name.to_string(), new_value);
    }
}
