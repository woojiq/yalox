use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    ops::{Add, Div, Mul, Neg, Not, Sub},
    rc::Rc,
};

use crate::{ast::stmt, scanner::token::Token};

use super::{environment::Environment, stmt::Class, Error, Interpreter};

#[derive(Clone, Copy, Debug)]
pub enum ValueOpError {
    Sub,
    Add,
    Div,
    Not,
    Neg,
    Mul,
}

impl fmt::Display for ValueOpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            ValueOpError::Sub => "subtraction",
            ValueOpError::Add => "addition",
            ValueOpError::Div => "division",
            ValueOpError::Not => "not operation",
            ValueOpError::Neg => "negative operation",
            ValueOpError::Mul => "multiplication",
        };
        write!(f, "wrong hands of {op}")
    }
}

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
            Value::Instance(v) => write!(f, "{} instance", v.class.name.lexeme()),
        }
    }
}

impl Neg for Value {
    type Output = Result<Value, ValueOpError>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Num(v) => Ok(Self::Num(-v)),
            _ => Err(ValueOpError::Neg),
        }
    }
}

impl Not for Value {
    type Output = Result<Value, ValueOpError>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(v) => Ok(Self::Bool(!v)),
            _ => Err(ValueOpError::Not),
        }
    }
}

impl Sub for Value {
    type Output = Result<Value, ValueOpError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Num(lhs), Self::Num(rhs)) => Ok(Self::Num(lhs - rhs)),
            _ => Err(ValueOpError::Sub),
        }
    }
}

impl Add for Value {
    type Output = Result<Value, ValueOpError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Num(lhs), Self::Num(rhs)) => Ok(Self::Num(lhs + rhs)),
            (Self::Str(lhs), Self::Str(rhs)) => Ok(Self::Str(lhs + &rhs)),
            _ => Err(ValueOpError::Add),
        }
    }
}

impl Div for Value {
    type Output = Result<Value, ValueOpError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Num(lhs), Self::Num(rhs)) => Ok(Self::Num(lhs / rhs)),
            _ => Err(ValueOpError::Div),
        }
    }
}

impl Mul for Value {
    type Output = Result<Value, ValueOpError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Num(lhs), Self::Num(rhs)) => Ok(Self::Num(lhs * rhs)),
            _ => Err(ValueOpError::Mul),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering;
        match (self, other) {
            (Value::Bool(l), Value::Bool(r)) => l.partial_cmp(r),
            (Value::Num(l), Value::Num(r)) => l.partial_cmp(r),
            (Value::Str(l), Value::Str(r)) => l.partial_cmp(r),
            (Value::Nil, Value::Nil) => Some(Ordering::Equal),
            _ => None,
        }
    }
}

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, Error>;
}

#[derive(Debug, Clone)]
pub struct Function {
    pub declaration: stmt::Function,
    pub closure: Rc<RefCell<Environment>>,
}

impl PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, Error> {
        let env = Rc::new(RefCell::new(Environment::new(self.closure.clone())));
        let mut env_mut = env.borrow_mut();
        for (param, arg) in self.declaration.params.iter().zip(args) {
            env_mut.define(param.lexeme(), arg);
        }
        drop(env_mut);
        match interpreter.execute_block(&self.declaration.body, env) {
            Ok(_) => Ok(Value::Nil),
            Err(Error::Return(val)) => Ok(val),
            Err(err) => Err(err),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    pub fun: fn(&mut Interpreter, Vec<Value>) -> Value,
}

impl PartialEq for NativeFunction {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Callable for NativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, Error> {
        Ok((self.fun)(interpreter, args))
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
}

impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        self.class.name == other.class.name && self.fields == other.fields
    }
}

impl Instance {
    pub fn get(&self, name: &Token) -> Value {
        todo!()
    }
}

impl Callable for Rc<Class> {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _interpreter: &mut Interpreter, _args: Vec<Value>) -> Result<Value, Error> {
        let instance = Instance {
            class: Rc::clone(self),
            fields: HashMap::default(),
        };
        Ok(Value::Instance(instance))
    }
}
