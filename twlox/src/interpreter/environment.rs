use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::value::PValue;

#[derive(Debug, Clone, Copy)]
pub enum Error {
    VarNotFound,
}

type Result<T = (), E = Error> = std::result::Result<T, E>;

/// Interpreter environment where we store variables, etc.
///
/// Each environment can have an enclosing environment
/// where we will look if not found in the current one.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Environment {
    values: HashMap<String, PValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: Default::default(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define<T: Into<PValue>>(&mut self, name: &str, value: T) {
        self.values.insert(name.to_string(), value.into());
    }

    pub fn get(&self, name: &str) -> Option<PValue> {
        if let Some(v) = self.values.get(name) {
            return Some(v.clone());
        }
        self.enclosing.as_ref().and_then(|s| s.borrow().get(name))
    }

    pub fn get_at(&self, name: &str, depth: usize) -> Option<PValue> {
        if depth == 0 {
            return self.values.get(name).cloned();
        }
        let Some(env) = self.enclosing.clone() else {
            return None;
        };
        let env_borrowed = env.borrow();
        env_borrowed.get_at(name, depth - 1)
    }

    pub fn assign<T: Into<PValue>>(&mut self, name: &str, value: T) -> Result {
        if let Some(v) = self.values.get_mut(name) {
            *v = value.into();
            Ok(())
        } else if self
            .enclosing
            .as_ref()
            .is_some_and(|s| s.borrow_mut().assign(name, value).is_ok())
        {
            Ok(())
        } else {
            Err(Error::VarNotFound)
        }
    }

    pub fn assign_at<T: Into<PValue>>(&mut self, name: &str, value: T, depth: usize) -> Result {
        if depth == 0 {
            if self.values.contains_key(name) {
                self.values.insert(name.to_string(), value.into());
                return Ok(());
            } else {
                return Err(Error::VarNotFound);
            }
        }
        let Some(env) = self.enclosing.clone() else {
            return Err(Error::VarNotFound);
        };
        let mut env_borrowed = env.borrow_mut();
        env_borrowed.assign_at(name, value, depth - 1)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::interpreter::Value;

    #[test]
    fn get_in_enclosing() {
        let min_lvl = 0;
        let max_lvl = 10;
        let field = "level";

        let mut base = Environment::default();
        base.define(field, Value::from(0 as f64).to_rc());
        for idx in min_lvl + 1..=max_lvl {
            base = Environment::new(Rc::new(RefCell::new(base)));
            base.define(field, Value::from(idx as f64).to_rc());
        }

        for lvl in min_lvl..=max_lvl {
            assert_eq!(
                base.get_at(field, max_lvl - lvl),
                Some(Value::from(lvl as f64).to_rc())
            );
        }
    }

    #[test]
    fn define_and_get_at_depth() {
        let min_lvl = 0;
        let max_lvl = 10;
        let field = "level";

        let mut base = Environment::default();
        base.define(field, Value::from(min_lvl as f64).to_rc());
        for lvl in min_lvl + 1..=max_lvl {
            base = Environment::new(Rc::new(RefCell::new(base)));
            base.define(field, Value::from(lvl as f64).to_rc());
        }
        for lvl in min_lvl..=max_lvl {
            assert_eq!(
                base.get_at(field, max_lvl - lvl),
                Some(Value::from(lvl as f64).to_rc())
            );

            base.assign_at(field, Value::from(-(lvl as f64)).to_rc(), max_lvl - lvl)
                .unwrap();

            assert_eq!(
                base.get_at(field, max_lvl - lvl),
                Some(Value::from(-(lvl as f64)).to_rc())
            );
        }
    }

    #[test]
    fn links_are_changed_too() {
        let mut env = Environment::default();
        let raw = false;
        let value = Value::from(raw).to_rc();
        env.define("first", value.clone());
        env.define("link_first", value.clone());

        assert_eq!(env.get("first"), Some(Value::from(raw).to_rc()));
        assert_eq!(env.get("link_first"), Some(Value::from(raw).to_rc()));

        *value.borrow_mut() = true.into();
        assert_eq!(env.get("first"), Some(Value::from(!raw).to_rc()));
        assert_eq!(env.get("link_first"), Some(Value::from(!raw).to_rc()));
    }

    #[test]
    fn modify_same_name_different_depth() {
        let field = "field";

        let base_env = Rc::new(RefCell::new(Environment::default()));
        let mut main_env = Environment::new(base_env.clone());

        base_env.borrow_mut().define(field, Value::from("val1"));
        main_env.define(field, Value::from("val2"));

        assert_eq!(main_env.get(field), Some(Value::from("val2").to_rc()));
        assert_eq!(main_env.get_at(field, 1), Some(Value::from("val1").to_rc()));

        base_env
            .borrow_mut()
            .assign(field, Value::from("val0"))
            .unwrap();
        assert_eq!(main_env.get_at(field, 1), Some(Value::from("val0").to_rc()));
    }

    #[test]
    fn op_with_not_existing_field() {
        let field = "field";

        let base_env = Rc::new(RefCell::new(Environment::default()));
        let mut main_env = Environment::new(base_env.clone());

        assert_eq!(main_env.get(field), None);
        assert_eq!(main_env.get_at(field, 1), None);
        assert!(main_env.assign(field, Value::from(0.)).is_err());
        assert!(main_env.assign_at(field, Value::from(0.), 1).is_err());
    }
    #[test]
    fn define_and_get_out_of_bounds() {
        let mut base = Environment::default();
        assert_eq!(base.get_at("", 1), None);
        assert!(base.assign_at("", Value::from("").to_rc(), 1).is_err());
    }
}
