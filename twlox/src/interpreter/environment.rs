use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::value::Value;

/// Interpreter environment where we store variables, etc.
///
/// Each environment can have an enclosing environment
/// where we will look if not found in the current one.
#[derive(Debug, Default, Clone)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: Default::default(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.values.get(name) {
            return Some(v.clone());
        }
        self.enclosing.as_ref().and_then(|s| s.borrow().get(name))
    }

    pub fn get_at(&self, name: &str, depth: usize) -> Option<Value> {
        if depth == 0 {
            return self.values.get(name).cloned();
        }
        let Some(env) = self.enclosing.clone() else {
            return None;
        };
        let env_borrowed = env.borrow();
        env_borrowed.get_at(name, depth - 1)
    }

    pub fn assign(&mut self, name: &str, value: Value) -> bool {
        if let Some(v) = self.values.get_mut(name) {
            *v = value;
            true
        } else {
            self.enclosing
                .as_ref()
                .is_some_and(|s| s.borrow_mut().assign(name, value))
        }
    }

    pub fn assign_at(&mut self, name: &str, value: Value, depth: usize) -> bool {
        if depth == 0 {
            return self.values.insert(name.to_string(), value).is_some();
        }
        let Some(env) = self.enclosing.clone() else {
            return false;
        };
        let mut env_borrowed = env.borrow_mut();
        env_borrowed.assign_at(name, value, depth - 1)
    }
}
