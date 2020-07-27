use crate::Event;
use std::collections::VecDeque;

pub(super) struct EventBuffer {
    limit: usize,
    vec: VecDeque<Event>,
}

impl EventBuffer {
    pub fn new(limit: usize) -> Self {
        Self {
            limit,
            vec: VecDeque::new(),
        }
    }

    pub fn push(&mut self, event: Event) {
        self.vec.push_back(event);
        if self.vec.len() > self.limit {
            self.vec.pop_front();
        }
    }

    pub fn clear(&mut self) -> Vec<Event> {
        std::mem::replace(&mut self.vec, VecDeque::new()).into()
    }
}
