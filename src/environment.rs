use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use std::u16;

use crate::value::*;

pub const VAR_LOC_MAX_DISTANCE: u16 = u16::MAX;
pub const VAR_LOC_MAX_DISTANCE_USIZE: usize = VAR_LOC_MAX_DISTANCE as usize;

pub const VAR_LOC_MAX_INDEX: u16 = u16::MAX;
pub const VAR_LOC_MAX_INDEX_USIZE: usize = VAR_LOC_MAX_INDEX as usize;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VarLoc {
    distance: u16,
    index: u16,
    unresolved: bool,
}

impl VarLoc {
    pub fn placeholder() -> VarLoc {
        VarLoc {
            distance: 0,
            index: 0,
            unresolved: true,
        }
    }

    pub fn new(distance: u16, index: u16) -> VarLoc {
        VarLoc {
            distance,
            index,
            unresolved: false,
        }
    }

    pub fn new_global(distance: u16, index: u16) -> VarLoc {
        VarLoc {
            distance,
            index,
            unresolved: false,
        }
    }

    pub fn distance(&self) -> u16 {
        self.distance
    }

    pub fn index(&self) -> u16 {
        self.index
    }
}

// We store values in a Vec so that lookups are fast.
#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    values: Vec<Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new_global() -> Environment {
        Environment {
            values: Vec::new(),
            enclosing: None,
        }
    }

    pub fn new_with_parent(enclosing: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            values: Vec::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, _name: &str, value: Value) -> u16 {
        let index = self.values.len();

        self.values.push(value);

        index as u16
    }

    pub fn get_at(&self, name: &str, var_loc: VarLoc) -> Option<Value> {
        if var_loc.unresolved {
            panic!("Environment::get_at(): var_loc should not be unresolved");
        }

        match self.get_at_distance(var_loc.index, var_loc.distance) {
            Err(true) => panic!("tried to look up a variable at a distance greater than exists: {} index={} distance={}", name, var_loc.index, var_loc.distance),
            Err(false) => None,
            Ok(v) => Some(v),
        }
    }

    // If there's a distance error (which is probably a bug in the resolver) a
    // true value is returned for the error.
    fn get_at_distance(&self, index: u16, distance: u16) -> Result<Value, bool> {
        if distance == 0 {
            return self.values.get(usize::from(index)).cloned().ok_or(false);
        }

        // TODO: implement this without recursion.
        match self.enclosing {
            None => Err(true),
            Some(ref env_cell_rc) => {
                let env = env_cell_rc.deref().borrow();

                env.get_at_distance(index, distance - 1)
            }
        }
    }

    // Returns an error result if the key isn't already defined.
    pub fn assign_at(&mut self, name: &str, var_loc: VarLoc, new_value: Value)
        -> Result<(), ()>
    {
        if var_loc.unresolved {
            panic!("Environment::assign_at(): var_loc should not be unresolved");
        }

        match self.assign_at_distance(var_loc.index, var_loc.distance, new_value) {
            Err(true) => panic!("tried to assign to a variable at a distance greater than exists: {} index={} distance={}", name, var_loc.index, var_loc.distance),
            Err(false) => Err(()),
            Ok(()) => Ok(())
        }
    }

    // Returns an error result if the key isn't already defined.
    fn assign_at_distance(&mut self, index: u16, distance: u16, new_value: Value)
        -> Result<(), bool>
    {
        if distance == 0 {
            let usize_index = usize::from(index);
            if usize_index >= self.values.len() {
                // Not found.
                return Err(false);
            }

            // Assign at this level.
            self.values[usize_index] = new_value;

            return Ok(());
        }

        // TODO: implement this without recursion.
        match self.enclosing {
            None => Err(true),
            Some(ref env_cell_rc) => {
                let mut env = env_cell_rc.deref().borrow_mut();

                env.assign_at_distance(index, distance - 1, new_value)
            }
        }
    }
}
