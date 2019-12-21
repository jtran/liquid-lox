use std::{u8, u16};

use generational_arena::{Arena, Index as ArenaIndex};

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

// An EnvironmentRef is an index in the arena.  It references a non-empty,
// singly-linked list of scopes.  Each scope is a mapping from SlotIndex keys to
// Values.
//
// EnvironmentRefs use Copy semantics; it is a cheap pointer copy.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct EnvironmentRef {
    layer_index: ArenaIndex,
}

impl EnvironmentRef {
    pub fn layer_index(&self) -> ArenaIndex {
        self.layer_index
    }
}

// We store values in a Vec so that lookups are fast.
#[derive(Clone, Debug, PartialEq)]
struct EnvironmentLayer {
    values: Vec<Option<Value>>,
    enclosing_index: Option<ArenaIndex>,
}

impl EnvironmentLayer {
    pub fn new_global() -> EnvironmentLayer {
        EnvironmentLayer {
            values: Vec::new(),
            enclosing_index: None,
        }
    }

    pub fn new_with_parent(enclosing: EnvironmentRef) -> EnvironmentLayer {
        EnvironmentLayer {
            values: Vec::new(),
            enclosing_index: Some(enclosing.layer_index()),
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

    pub fn get(&self, slot_index: u8) -> Option<Option<Value>> {
        self.values.get(usize::from(slot_index)).cloned()
    }

    pub fn assign_at(&mut self, index: u8, new_value: Value) -> Result<(), ()> {
        let usize_index = usize::from(index);
        if usize_index >= self.values.len() {
            // Not found.
            return Err(());
        }

        self.values[usize_index] = Some(new_value);

        return Ok(());
    }
}

#[derive(Debug)]
pub struct EnvironmentManager {
    layers: Arena<EnvironmentLayer>,
}

impl EnvironmentManager {
    pub fn with_capacity(capacity: usize) -> EnvironmentManager {
        EnvironmentManager {
            layers: Arena::with_capacity(capacity),
        }
    }

    pub fn new_global(&mut self) -> EnvironmentRef {
        assert!(self.layers.is_empty());

        let layer = EnvironmentLayer::new_global();
        let layer_index = self.layers.insert(layer);

        EnvironmentRef { layer_index }
    }

    pub fn new_with_parent(&mut self, enclosing: EnvironmentRef) -> EnvironmentRef {
        let layer = EnvironmentLayer::new_with_parent(enclosing);
        let layer_index = self.layers.insert(layer);

        EnvironmentRef { layer_index }
    }

    pub fn next_slot_index(&self, env_ref: EnvironmentRef) -> SlotIndex {
        self.layers[env_ref.layer_index()].next_slot_index()
    }

    pub fn num_layers(&self) -> usize {
        self.layers.len()
    }

    pub fn enclosing_env(&self, env_ref: &EnvironmentRef) -> Option<EnvironmentRef> {
        let layer = &self.layers[env_ref.layer_index()];

        layer.enclosing_index.map(|index| EnvironmentRef { layer_index: index })
    }

    // Iterate over slots in the single given layer.
    pub fn layer_slots(&self, env_ref: &EnvironmentRef) -> impl Iterator<Item = &Option<Value>> + '_ {
        self.layers[env_ref.layer_index()].values.iter()
    }

    // Iterate over all environment layers for memory recycling.  Order is
    // unspecified.
    pub fn env_iter(&mut self) -> impl Iterator<Item = EnvironmentRef> + '_ {
        self.layers.iter()
            .map(|(layer_index, _layer)| EnvironmentRef { layer_index })
    }

    pub fn free(&mut self, env_ref: EnvironmentRef) {
        self.layers.remove(env_ref.layer_index());
    }

    pub fn define_at(&mut self, env_ref: EnvironmentRef, name: &str, slot_index: SlotIndex, value: Value) {
        let layer = &mut self.layers[env_ref.layer_index()];
        layer.define_at(name, slot_index, value);
    }

    pub fn get_at(&self, env_ref: EnvironmentRef, name: &str, var_loc: VarLoc) -> Option<Value> {
        let mut distance = var_loc.distance;
        let mut layer_index = env_ref.layer_index();
        loop {
            let layer = &self.layers[layer_index];
            if distance == 0 {
                return match layer.get(var_loc.index) {
                    None => None,
                    Some(None) => None,
                    Some(Some(v)) => Some(v.clone())
                };
            }

            match layer.enclosing_index {
                // If we hit this, it's probably a bug in the resolver.
                None => panic!("tried to look up a variable at a distance greater than exists: {} var_loc={:?}", name, var_loc),
                Some(encl_index) => {
                    layer_index = encl_index;
                    distance = distance - 1;
                }
            }
        }
    }

    // Returns an error result if the key isn't already defined.
    pub fn assign_at(&mut self, env_ref: EnvironmentRef, name: &str, var_loc: VarLoc, new_value: Value)
        -> Result<(), ()>
    {
        let mut distance = var_loc.distance;
        let mut layer_index = env_ref.layer_index();
        loop {
            let layer = &mut self.layers[layer_index];
            if distance == 0 {
                // Assign at this level.
                return layer.assign_at(var_loc.index, new_value);
            }

            match layer.enclosing_index {
                None => panic!("tried to assign to a variable at a distance greater than exists: {} var_loc={:?}", name, var_loc),
                Some(encl_index) => {
                    layer_index = encl_index;
                    distance = distance - 1;
                }
            }
        }
    }
}
