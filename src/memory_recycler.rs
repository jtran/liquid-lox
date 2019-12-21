use fnv::FnvHashMap;

use crate::environment::*;
use crate::value::Value;

// Set this to true to print debug info when collection is run.
const DEBUG_LOG: bool = false;

// A memory recycler frees memory that can no longer be used by the program in
// the future so that the memory can be reused.  In most literature, this is
// called a garbage collector, or GC for short.
//
// We currently only have one implementation, but this is a trait in the hopes
// that we'll have a clean interface and more implementations in the future.
pub trait MemoryRecycler {
    fn collect(&mut self,
               env_mgr: &mut EnvironmentManager,
               root_envs: &[EnvironmentRef]);
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum RecyclerMark {
    Visited,
}

// This is a simple mark and sweep collector.
//
// Typically, the marks are stored on the allocations themselves, which is
// intrusive.  We're not doing that.  It would require mutating the allocations
// while iterating over them, and Rust's borrowing rules discourage that.
//
// Instead, all state is stored here.  The state is transient and only needed
// during a single collection.  But we're using members on the struct to avoid
// re-allocating the temp space every collection.  Whether or not things are
// re-allocated depends on their implementations.
pub struct MarkAndSweepRecycler {
    to_visit: Vec<EnvironmentRef>,
    marks: FnvHashMap<EnvironmentRef, RecyclerMark>,
    unreachable: Vec<EnvironmentRef>,
}

impl MarkAndSweepRecycler {
    pub fn with_capacity(capacity: usize) -> MarkAndSweepRecycler {
        MarkAndSweepRecycler {
            to_visit: Vec::with_capacity(capacity),
            marks: FnvHashMap::default(),
            unreachable: Vec::with_capacity(capacity),
        }
    }

    fn mark_roots(&mut self, root_envs: &[EnvironmentRef]) {
        if DEBUG_LOG {
            eprintln!("  Marking roots");
        }
        for env in root_envs {
            self.mark_env(*env);
        }
    }

    fn mark_env(&mut self, env: EnvironmentRef) {
        let mut is_new = false;
        self.marks.entry(env).or_insert_with(|| {
            is_new = true;

            RecyclerMark::Visited
        });
        if DEBUG_LOG {
            eprintln!("  Marking env is_new={:?}, env={:?}", is_new, env);
        }
        if is_new {
            self.to_visit.push(env);
        }
    }

    fn trace_references(&mut self, env_mgr: &mut EnvironmentManager) {
        if DEBUG_LOG {
            eprintln!("  Tracing references");
        }
        loop {
            match self.to_visit.pop() {
                None => break,
                Some(env) => {
                    self.trace_env(env_mgr, env);
                }
            }
        }
        assert!(self.to_visit.is_empty());
    }

    fn trace_env(&mut self, env_mgr: &EnvironmentManager, env: EnvironmentRef) {
        // TODO: This is conservative.  Not all things in enclosing environments
        // may be reachable.  They may be shadowed or simply never referenced.
        // We're currently assuming that everything in a scope is reachable, and
        // everything in all its enclosing scopes is also reachable.
        if let Some(enclosing_env) = env_mgr.enclosing_env(&env) {
            self.mark_env(enclosing_env);
        }
        for value_opt in env_mgr.layer_slots(&env) {
            if let Some(value) = value_opt {
                self.trace_value(env_mgr, &value);
            }
        }
    }

    fn trace_value(&mut self, env_mgr: &EnvironmentManager, value: &Value) {
        if DEBUG_LOG {
            eprintln!("  Tracing value: {:?}", value);
        }
        match value {
            Value::BoolVal(_)
            | Value::NativeFunctionVal(_)
            | Value::NilVal
            | Value::NumberVal(_)
            | Value::StringVal(_) => (),
            Value::ArrayVal(vec_ref) => {
                for v in (*vec_ref).borrow().iter() {
                    self.trace_value(env_mgr, v);
                }
            }
            Value::ClassVal(class_ref) => {
                if let Some(superclass) = class_ref.superclass() {
                    self.trace_value(env_mgr, &Value::ClassVal(superclass));
                }
                // We need to get the Rc instance so that it lives long enough
                // that we can iterate over it.
                let class_rc = class_ref.to_rc();
                for value in class_rc.borrow().field_values_iter() {
                    self.trace_value(env_mgr, value);
                }
                for value in class_rc.borrow().method_values_iter() {
                    self.trace_value(env_mgr, value);
                }
            }
            Value::ClosureVal(closure_ref) => {
                self.mark_env(*closure_ref.env())
            }
            Value::InstanceVal(instance_ref) => {
                self.trace_value(env_mgr, &Value::ClassVal(instance_ref.class()));
                // We need to get the Rc instance so that it lives long enough
                // that we can iterate over it.
                let instance_rc = instance_ref.to_rc();
                for value in instance_rc.borrow().field_values_iter() {
                    self.trace_value(env_mgr, value);
                }
            }
        }
    }

    // This is known as "sweep" in the literature.
    fn free_unreachable(&mut self, env_mgr: &mut EnvironmentManager) {
        if DEBUG_LOG {
            eprintln!("  Freeing unreachable");
        }
        let mut num_reachable: usize = 0;
        for env_ref in env_mgr.env_iter() {
            if self.marks.get(&env_ref) == Some(&RecyclerMark::Visited) {
                num_reachable = num_reachable.saturating_add(1);
            } else {
                self.unreachable.push(env_ref);
            }
        }
        if DEBUG_LOG {
            eprintln!("  Found {} reachable; {} unreachable", num_reachable, self.unreachable.len());
        }
        self.marks.clear();
        for env_ref in self.unreachable.drain(..) {
            if DEBUG_LOG {
                eprintln!("  Freeing {:?}", env_ref);
            }
            env_mgr.free(env_ref);
        }
    }
}

impl MemoryRecycler for MarkAndSweepRecycler {
    fn collect(&mut self,
               env_mgr: &mut EnvironmentManager,
               root_envs: &[EnvironmentRef]) {
        self.mark_roots(root_envs);
        self.trace_references(env_mgr);
        // TODO: When we add weak referenes, set them to null here.
        self.free_unreachable(env_mgr);
    }
}
