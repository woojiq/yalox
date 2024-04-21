use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{ast::stmt, scanner::token::Token};

use super::{environment::Environment, Error, Interpreter, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Num(f64),
    Str(String),
    Function(Function),
    NativeFunction(NativeFunction),
    Class(Class),
    Instance(Instance),
    Nil,
}

pub type PValue = Rc<RefCell<Value>>;

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }

    pub fn to_rc(self) -> PValue {
        self.into()
    }
}

impl From<Value> for PValue {
    fn from(value: Value) -> Self {
        Rc::new(RefCell::new(value))
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Num(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::Str(value.into())
    }
}

impl From<Value> for bool {
    fn from(val: Value) -> Self {
        !(matches!(val, Value::Bool(false) | Value::Nil))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(v) => write!(f, "{v}"),
            Value::Num(v) => write!(f, "{v}"),
            Value::Str(v) => write!(f, "{v}"),
            Value::Nil => write!(f, "nil"),
            Value::Function(v) => write!(f, "<fun>{}", v.declaration.name.lexeme()),
            Value::NativeFunction(v) => write!(f, "<fun>{}", v.name),
            Value::Class(v) => write!(f, "<class>{}", v.name.lexeme()),
            Value::Instance(v) => write!(f, "<instance>{}", v.class.name.lexeme()),
        }
    }
}

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: Vec<PValue>) -> Result;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub declaration: stmt::Function,
    pub closure: Rc<RefCell<Environment>>,
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<PValue>) -> Result {
        let env = Rc::new(RefCell::new(Environment::new(self.closure.clone())));
        let mut env_mut = env.borrow_mut();
        for (param, arg) in self.declaration.params.iter().zip(args) {
            env_mut.define(param.lexeme(), arg);
        }
        drop(env_mut);
        match interpreter.execute_block(&self.declaration.body, env) {
            Ok(_) => Ok(Value::Nil.to_rc()),
            Err(Error::Return(val)) => Ok(val),
            Err(err) => Err(err),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    pub fun: fn(&mut Interpreter, Vec<PValue>) -> Value,
}

impl Callable for NativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<PValue>) -> Result {
        Ok((self.fun)(interpreter, args).to_rc())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    pub class: Class,
    pub fields: HashMap<String, PValue>,
}

impl Instance {
    pub fn get(&self, name: &Token) -> Option<PValue> {
        if self.fields.contains_key(name.lexeme()) {
            return self.fields.get(name.lexeme()).cloned();
        }
        self.class
            .methods
            .get(name.lexeme())
            .map(|func| Rc::new(RefCell::new(Value::Function(func.borrow().clone()))))
    }

    pub fn set(&mut self, name: &Token, val: PValue) {
        self.fields.insert(name.lexeme().to_owned(), val);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: Token,
    pub methods: HashMap<String, Rc<RefCell<Function>>>,
}

impl Callable for Class {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _interpreter: &mut Interpreter, _args: Vec<PValue>) -> Result {
        let instance = Instance {
            class: self.clone(),
            fields: HashMap::default(),
        };
        Ok(Value::Instance(instance).to_rc())
    }
}
