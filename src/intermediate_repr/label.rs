//! IR Labels for control flow

use std::cell::Cell;
use std::fmt;

/// A label in the IR for control flow jumps
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub struct Label(usize);

/// An allocator of Labels, used during translation.
#[derive(Debug, Clone)]
pub struct LabelAllocator {
    next: Cell<usize>,
}

impl LabelAllocator {
    /// Prepares a new allocator ready to create new labels
    pub fn new() -> LabelAllocator {
        LabelAllocator { next: Cell::new(0) }
    }

    /// Returns the number of labels allocated so far
    pub fn count(&self) -> usize {
        self.next.get()
    }

    /// Resets the allocator back to 0
    pub fn reset(&self) {
        self.next.set(0);
    }

    /// Generates a new unique label
    pub fn generate(&self) -> Label {
        let ret = self.next.get();
        self.next.set(ret + 1);
        Label(ret)
    }

    /// Generates a new unique label with a descriptive name
    pub fn generate_named(&self, name: &str) -> Label {
        let ret = self.next.get();
        self.next.set(ret + 1);
        Label(ret)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "L{}", self.0)
    }
}

impl fmt::Debug for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "L{}", self.0)
    }
}