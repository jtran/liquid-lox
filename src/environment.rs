use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use std::{u8, u16};

use crate::value::*;

pub const VAR_LOC_MAX_DISTANCE: u16 = u16::MAX;
pub const VAR_LOC_MAX_DISTANCE_USIZE: usize = VAR_LOC_MAX_DISTANCE as usize;

pub const VAR_LOC_MAX_INDEX: u8 = u8::MAX;
pub const VAR_LOC_MAX_INDEX_USIZE: usize = VAR_LOC_MAX_INDEX as usize;

// When you declare a variable, you need to know where in the current frame it
// is stored in memory.  You cannot declare a new variable in a distant frame.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FrameIndex(u8);

impl FrameIndex {
    pub fn new(index: u8) -> FrameIndex {
        FrameIndex(index)
    }

    pub fn placeholder() -> FrameIndex {
        FrameIndex(u8::MAX)
    }

    pub fn index(&self) -> u8 {
        self.0
    }

    pub fn next(&self) -> Option<FrameIndex> {
        match self.0.checked_add(1) {
            None => None,
            Some(new_index) => Some(FrameIndex::new(new_index)),
        }
    }
}

// When you use an already-declared variable, you need to know which frame it is
// in, and where in the frame it is.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VarLoc {
    distance: u16,
    index: u8,
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

    pub fn new(distance: u16, index: u8) -> VarLoc {
        VarLoc {
            distance,
            index,
            unresolved: false,
        }
    }

    pub fn new_global(distance: u16, index: u8) -> VarLoc {
        VarLoc {
            distance,
            index,
            unresolved: false,
        }
    }

    pub fn distance(&self) -> u16 {
        self.distance
    }

    pub fn index(&self) -> u8 {
        self.index
    }
}

impl From<FrameIndex> for VarLoc {
    fn from(frame_index: FrameIndex) -> VarLoc {
        VarLoc::new(0, frame_index.index())
    }
}

impl From<&VarLoc> for FrameIndex {
    fn from(var_loc: &VarLoc) -> FrameIndex {
        FrameIndex::new(var_loc.index())
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

    pub fn next_frame_index(&self) -> FrameIndex {
        let len = self.values.len();
        // Be carefult to compare with a larger width.  But the resolver should
        // have prevented this.
        if len > usize::from(u8::MAX) {
            panic!("Environment::next_frame_index(): frame overflow");
        }

        FrameIndex::new(len as u8)
    }

    pub fn define_at(&mut self, name: &str, frame_index: FrameIndex, value: Value) {
        let index = usize::from(frame_index.index());
        let len = self.values.len();

        if index == len {
            self.values.push(value);
        } else if index < len {
            self.values[index] = value;
        } else {
            panic!("Environment::define_at(): You tried to define at a higher index which would create a gap: len={}, index={}, name={}", len, index, name);
        }
    }

    pub fn get_at(&self, name: &str, var_loc: VarLoc) -> Option<Value> {
        if var_loc.unresolved {
            panic!("Environment::get_at(): var_loc should not be unresolved");
        }

        match self.get_at_distance(var_loc.index, var_loc.distance) {
            Err(LookupFailure::DistanceTooFar) => panic!("tried to look up a variable at a distance greater than exists: {} index={} distance={}", name, var_loc.index, var_loc.distance),
            Err(LookupFailure::NotDefined) => None,
            Ok(v) => Some(v),
        }
    }

    // If there's a distance error, it's probably a bug in the resolver.
    fn get_at_distance(&self, index: u8, distance: u16) -> Result<Value, LookupFailure> {
        if distance == 0 {
            return self.values.get(usize::from(index)).cloned().ok_or(LookupFailure::NotDefined);
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
        if var_loc.unresolved {
            panic!("Environment::assign_at(): var_loc should not be unresolved");
        }

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
            self.values[usize_index] = new_value;

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
