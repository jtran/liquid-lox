use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use std::{u8, u16};

use crate::value::*;

pub const VAR_LOC_MAX_DISTANCE: u16 = u16::MAX;
pub const VAR_LOC_MAX_DISTANCE_USIZE: usize = VAR_LOC_MAX_DISTANCE as usize;

pub const VAR_LOC_MAX_INDEX: u8 = u8::MAX;
pub const VAR_LOC_MAX_INDEX_USIZE: usize = VAR_LOC_MAX_INDEX as usize;

// When you declare a variable, you need to know where in the scope it is stored
// in memory.  We currently use a slot index since a scope is implemented with
// an environment, which has a vector of slots.  You cannot currently declare
// a new variable in a distant scope.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SlotIndex(u8);

impl SlotIndex {
    pub fn new(index: u8) -> SlotIndex {
        SlotIndex(index)
    }

    pub fn placeholder() -> SlotIndex {
        SlotIndex(u8::MAX)
    }

    pub fn index(&self) -> u8 {
        self.0
    }

    pub fn next(&self) -> Option<SlotIndex> {
        match self.0.checked_add(1) {
            None => None,
            Some(new_index) => Some(SlotIndex::new(new_index)),
        }
    }
}

// When you use an already-declared variable, you need to know which scope it is
// in (distance), and where in the scope it is (index).
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VarLoc {
    distance: u16,
    index: u8,
}

impl VarLoc {
    pub fn placeholder() -> VarLoc {
        VarLoc {
            distance: 0,
            index: 0,
        }
    }

    pub fn new(distance: u16, index: u8) -> VarLoc {
        VarLoc {
            distance,
            index,
        }
    }

    pub fn new_global(distance: u16, index: u8) -> VarLoc {
        VarLoc {
            distance,
            index,
        }
    }

    pub fn distance(&self) -> u16 {
        self.distance
    }

    pub fn index(&self) -> u8 {
        self.index
    }
}

impl From<SlotIndex> for VarLoc {
    fn from(slot_index: SlotIndex) -> VarLoc {
        VarLoc::new(0, slot_index.index())
    }
}

impl From<&VarLoc> for SlotIndex {
    fn from(var_loc: &VarLoc) -> SlotIndex {
        SlotIndex::new(var_loc.index())
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum LookupFailure {
    NotDefined,
    DistanceTooFar,
}

// We store values in a Vec so that lookups are fast.
#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    values: Vec<Option<Value>>,
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

    pub fn new_for_call(enclosing: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            values: Vec::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn next_slot_index(&self) -> SlotIndex {
        let len = self.values.len();
        // Be carefult to compare with a larger width.  But the resolver should
        // have prevented this.
        if len > usize::from(u8::MAX) {
            panic!("Environment::next_slot_index(): scope overflow");
        }

        SlotIndex::new(len as u8)
    }

    pub fn define_at(&mut self, _name: &str, slot_index: SlotIndex, value: Value) {
        let index = usize::from(slot_index.index());
        let mut len = self.values.len();

        loop {
            if index == len {
                self.values.push(Some(value));
                break;
            } else if index < len {
                self.values[index] = Some(value);
                break;
            }

            // We're defining after a gap, so we need to fill the gap.
            self.values.push(None);
            len += 1;
        }
    }

    pub fn get_at(&self, name: &str, var_loc: VarLoc) -> Option<Value> {
        match self.get_at_distance(var_loc.index, var_loc.distance) {
            Err(LookupFailure::DistanceTooFar) => panic!("tried to look up a variable at a distance greater than exists: {} index={} distance={}", name, var_loc.index, var_loc.distance),
            Err(LookupFailure::NotDefined) => None,
            Ok(v) => Some(v),
        }
    }

    // If there's a distance error, it's probably a bug in the resolver.
    fn get_at_distance(&self, index: u8, distance: u16) -> Result<Value, LookupFailure> {
        if distance == 0 {
            return match self.values.get(usize::from(index)) {
                None => Err(LookupFailure::NotDefined),
                Some(None) => Err(LookupFailure::NotDefined),
                Some(Some(v)) => Ok(v.clone())
            };
        }

        // TODO: implement this without recursion.
        match self.enclosing {
            None => Err(LookupFailure::DistanceTooFar),
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
        match self.assign_at_distance(var_loc.index, var_loc.distance, new_value) {
            Err(LookupFailure::DistanceTooFar) => panic!("tried to assign to a variable at a distance greater than exists: {} index={} distance={}", name, var_loc.index, var_loc.distance),
            Err(LookupFailure::NotDefined) => Err(()),
            Ok(()) => Ok(())
        }
    }

    // Returns an error result if the key isn't already defined.
    fn assign_at_distance(&mut self, index: u8, distance: u16, new_value: Value)
        -> Result<(), LookupFailure>
    {
        if distance == 0 {
            let usize_index = usize::from(index);
            if usize_index >= self.values.len() {
                // Not found.
                return Err(LookupFailure::NotDefined);
            }

            // Assign at this level.
            self.values[usize_index] = Some(new_value);

            return Ok(());
        }

        // TODO: implement this without recursion.
        match self.enclosing {
            None => Err(LookupFailure::DistanceTooFar),
            Some(ref env_cell_rc) => {
                let mut env = env_cell_rc.deref().borrow_mut();

                env.assign_at_distance(index, distance - 1, new_value)
            }
        }
    }
}
