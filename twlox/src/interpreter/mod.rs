// TODO: Use Rc::<method> instead of <variable>.<method>
// https://doc.rust-lang.org/std/rc/struct.Rc.html
pub mod environment;
pub mod value;

use crate::{
    ast::{
        expr::{self, Expr, ExprVisitor},
        stmt::{self, Stmt, StmtVisitor},
    },
    scanner::token::{Token, TokenType},
    Position,
};

use core::fmt;
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use self::{
    environment::Environment,
    value::{Callable, Function, NativeFunction, PValue, Value},
};

#[derive(Debug, Clone)]
pub enum Error {
    UndefinedVariable {
        tok: Token,
    },
    UndefinedProperty {
        tok: Token,
    },
    Arity {
        tok: Token,
    },
    ConditionExpected {
        stmt: stmt::If,
    },
    PropertyNotOnObj {
        property: Token,
    },
    InvalidOperand {
        op: Token,
        expected: &'static str,
        found: Value,
    },
    DivisionByZero {
        op: Token,
    },

    // === Not exactly Errors === //
    Return(PValue),
}

type Result<T = PValue, E = Error> = std::result::Result<T, E>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UndefinedVariable { tok } => {
                write!(f, "undefined variable {} at {}", tok.lexeme(), tok.pos())
            }
            Error::Arity { tok } => write!(f, "wrong arity at {}", tok.pos()),
            Error::ConditionExpected { stmt } => {
                write!(f, "expected boolean in condition: {}", stmt.condition)
            }
            Error::Return(_) => unreachable!(),
            Error::PropertyNotOnObj { property: prop } => {
                write!(f, "only instances have properties at {}", prop.pos())
            }
            Error::UndefinedProperty { tok } => {
                write!(f, "undefined property {} at {}", tok.lexeme(), tok.pos())
            }
            Error::InvalidOperand {
                op,
                expected,
                found,
            } => {
                write!(
                    f,
                    "'{}' expected {}, found {} at {}",
                    op.lexeme(),
                    expected,
                    found,
                    op.pos()
                )
            }
            Error::DivisionByZero { op } => write!(f, "division by zero at {}", op.pos()),
        }
    }
}

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    /// Depth of a variable at specific location.
    locals: HashMap<Position, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::default()));
        globals.borrow_mut().define(
            "clock",
            Value::NativeFunction(NativeFunction {
                name: "clock".to_string(),
                arity: 0,
                fun: |_, _| {
                    Value::Num(
                        SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_secs_f64(),
                    )
                },
            })
            .to_rc(),
        );

        Self {
            env: globals.clone(),
            globals,
            locals: HashMap::default(),
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<()> {
        for stmt in statements {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn evaluate(&mut self, expr: &Expr) -> Result {
        expr.accept(self)
    }

    fn execute(&mut self, statement: &Stmt) -> Result<()> {
        statement.accept(self)
    }

    pub fn execute_block(
        &mut self,
        statements: &[Stmt],
        env: Rc<RefCell<Environment>>,
    ) -> Result<()> {
        let prev_env = self.env.clone();
        self.env = env;
        let res = self.interpret(statements);
        self.env = prev_env;
        res
    }

    // TODO: remove when integration tests are moved from "runner.rs".
    pub fn env(&self) -> Ref<'_, Environment> {
        self.env.borrow()
    }

    pub fn resolve(&mut self, entry: crate::resolver::ResolvedEntry) {
        self.locals.insert(entry.pos, entry.depth);
    }

    fn lookup_variable(&self, token: Token) -> Result {
        if let Some(depth) = self.locals.get(&token.pos()) {
            self.env.borrow().get_at(token.lexeme(), *depth)
        } else {
            self.globals.borrow().get(token.lexeme())
        }
        .ok_or(Error::UndefinedVariable { tok: token })
        .map(Into::into)
    }

    fn bind_this(&self, func: &Function, instance: PValue) -> Function {
        if let Value::Instance(_) = &*instance.borrow() {
            let mut closure = Environment::new(self.env.clone());
            closure.define("this", instance.clone());
            Function {
                declaration: func.declaration.clone(),
                closure: Rc::new(RefCell::new(closure)),
                is_init: func.is_init,
            }
        } else {
            panic!("bind_this on not an instance");
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl ExprVisitor<Result> for Interpreter {
    fn visit_binary(&mut self, expr: &expr::Binary) -> Result {
        let lhs_rc = self.evaluate(&expr.left)?;
        let rhs_rc = self.evaluate(&expr.right)?;
        let lhs = &*lhs_rc.borrow();
        let rhs = &*rhs_rc.borrow();
        match expr.op.get_type() {
            TokenType::Minus => match (lhs, rhs) {
                (Value::Num(l), Value::Num(r)) => Ok(Value::Num(l - r).to_rc()),
                (Value::Num(_), v) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
                (v, _) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
            },
            TokenType::Plus => match (lhs, rhs) {
                (Value::Num(l), Value::Num(r)) => Ok(Value::Num(l + r).to_rc()),
                (Value::Num(_), v) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Str(l.to_owned() + r).to_rc()),
                (Value::Str(_), v) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "String",
                    found: v.clone(),
                }),
                (v, _) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number or String",
                    found: v.clone(),
                }),
            },
            TokenType::Slash => match (lhs, rhs) {
                (Value::Num(_), Value::Num(r)) if *r == 0.0 => Err(Error::DivisionByZero {
                    op: expr.op.clone(),
                }),
                (Value::Num(l), Value::Num(r)) => Ok(Value::Num(l / r).to_rc()),
                (v, _) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
            },
            TokenType::Star => match (lhs, rhs) {
                (Value::Num(l), Value::Num(r)) => Ok(Value::Num(l * r).to_rc()),
                (v, _) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
            },
            TokenType::BangEqual => Ok(Value::Bool(lhs != rhs).to_rc()),
            TokenType::EqualEqual => Ok(Value::Bool(lhs == rhs).to_rc()),
            TokenType::Greater => match (lhs, rhs) {
                (Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l > r).to_rc()),
                (Value::Num(_), v) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
                (v, _) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
            },
            TokenType::GreaterEqual => match (lhs, rhs) {
                (Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l >= r).to_rc()),
                (Value::Num(_), v) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
                (v, _) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
            },
            TokenType::Less => match (lhs, rhs) {
                (Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l < r).to_rc()),
                (Value::Num(_), v) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
                (v, _) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
            },
            TokenType::LessEqual => match (lhs, rhs) {
                (Value::Num(l), Value::Num(r)) => Ok(Value::Bool(l <= r).to_rc()),
                (Value::Num(_), v) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
                (v, _) => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
            },
            TokenType::False => Ok(Value::from(false).to_rc()),
            TokenType::True => Ok(Value::from(true).to_rc()),
            _ => unreachable!(),
        }
    }

    fn visit_grouping(&mut self, expr: &expr::Grouping) -> Result {
        self.evaluate(&expr.inner)
    }

    fn visit_literal(&mut self, expr: &expr::Literal) -> Result {
        Ok(match expr {
            expr::Literal::Number(n) => Value::Num(*n),
            expr::Literal::String(s) => Value::Str(s.to_string()),
            expr::Literal::Bool(b) => Value::Bool(*b),
            expr::Literal::Nil => Value::Nil,
        }
        .into())
    }

    fn visit_unary(&mut self, expr: &expr::Unary) -> Result {
        let val_rc = self.evaluate(&expr.inner)?;
        let val = &*val_rc.borrow();
        match expr.op.get_type() {
            TokenType::Minus => match val {
                Value::Num(val) => Ok(Value::Num(-val).to_rc()),
                v => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Number",
                    found: v.clone(),
                }),
            },
            TokenType::Bang => match val {
                Value::Bool(val) => Ok(Value::Bool(!val).to_rc()),
                v => Err(Error::InvalidOperand {
                    op: expr.op.clone(),
                    expected: "Bool",
                    found: v.clone(),
                }),
            },
            _ => unreachable!(),
        }
    }

    fn visit_variable(&mut self, expr: &expr::Variable) -> Result {
        self.lookup_variable(expr.name.clone())
    }

    fn visit_assign(&mut self, expr: &expr::Assign) -> Result {
        let value = self.evaluate(&expr.value)?;
        let status = if let Some(&depth) = self.locals.get(&expr.name.pos()) {
            self.env
                .borrow_mut()
                .assign_at(expr.name.lexeme(), value.clone(), depth)
        } else {
            self.env
                .borrow_mut()
                .assign(expr.name.lexeme(), value.clone())
        };
        match status {
            Ok(()) => Ok(value),
            Err(environment::Error::VarNotFound) => Err(Error::UndefinedVariable {
                tok: expr.name.clone(),
            }),
        }
    }

    fn visit_logical(&mut self, expr: &expr::Logical) -> Result {
        let lhs = self.evaluate(&expr.left)?;
        match expr.op.get_type() {
            TokenType::And if !lhs.borrow().is_truthy() => Ok(lhs),
            TokenType::OR if lhs.borrow().is_truthy() => Ok(lhs),
            _ => self.evaluate(&expr.right),
        }
    }

    fn visit_call(&mut self, expr: &expr::Call) -> Result {
        let callee_rc = self.evaluate(&expr.callee)?;
        let callee = &*callee_rc.borrow();
        let callable: &dyn Callable = match callee {
            Value::Function(f) => f as &dyn Callable,
            Value::NativeFunction(f) => f as &dyn Callable,
            Value::Class(f) => f as &dyn Callable,
            _ => unreachable!(),
        };

        let args = expr
            .arguments
            .iter()
            .map(|a| self.evaluate(a))
            .collect::<Result<Vec<_>>>()?;

        if args.len() != callable.arity() {
            return Err(Error::Arity {
                tok: expr.closing_paren.clone(),
            });
        }
        callable.call(self, args)
    }

    fn visit_get(&mut self, expr: &expr::Get) -> Result {
        let obj_rc = self.evaluate(&expr.obj)?;
        let obj = obj_rc.borrow();
        let name = expr.name.lexeme();
        if let Value::Instance(val) = &*obj {
            if let Some(var) = val.fields.get(name) {
                return Ok(var.clone());
            }
            if let Some(method) = val.class.methods.get(name) {
                return Ok(
                    Value::Function(self.bind_this(&method.borrow(), obj_rc.clone())).to_rc(),
                );
            }
            Err(Error::UndefinedProperty {
                tok: expr.name.clone(),
            })
        } else {
            Err(Error::PropertyNotOnObj {
                property: expr.name.clone(),
            })
        }
    }

    fn visit_set(&mut self, expr: &expr::Set) -> Result {
        let obj_rc = self.evaluate(&expr.obj)?;
        if !matches!(*obj_rc.borrow(), Value::Instance(_)) {
            return Err(Error::PropertyNotOnObj {
                property: expr.name.clone(),
            });
        }
        let value = self.evaluate(&expr.value)?;
        let mut obj = obj_rc.borrow_mut();
        if let Value::Instance(val) = &mut *obj {
            val.set(&expr.name, value.clone());
        } else {
            unreachable!("we checked above that the object is an instance");
        }
        Ok(value)
    }

    fn visit_this(&mut self, expr: &expr::This) -> Result {
        self.lookup_variable(expr.keyword.clone())
    }
}

impl StmtVisitor<Result<()>> for Interpreter {
    fn visit_block(&mut self, stmt: &stmt::Block) -> Result<()> {
        self.execute_block(
            &stmt.statements,
            Rc::new(RefCell::new(Environment::new(self.env.clone()))),
        )
    }

    fn visit_class(&mut self, stmt: &stmt::Class) -> Result<()> {
        self.env
            .borrow_mut()
            .define(stmt.name.lexeme(), Value::Nil.to_rc());
        let mut methods = HashMap::default();
        for method in &stmt.methods {
            let func = Function {
                declaration: method.clone(),
                closure: self.env.clone(),
                is_init: method.name.lexeme() == "init",
            };
            methods.insert(
                method.name.lexeme().to_string(),
                Rc::new(RefCell::new(func)),
            );
        }
        let class = Value::Class(value::Class {
            name: stmt.name.clone(),
            methods,
        });
        self.env
            .borrow_mut()
            .assign(stmt.name.lexeme(), class.to_rc())
            .map_err(|_| Error::UndefinedVariable {
                tok: stmt.name.clone(),
            })
    }

    fn visit_expression(&mut self, stmt: &stmt::Expression) -> Result<()> {
        self.evaluate(&stmt.expression).map(|_| ())
    }

    fn visit_function(&mut self, stmt: &stmt::Function) -> Result<()> {
        let function = Value::Function(Function {
            declaration: stmt.clone(),
            closure: self.env.clone(),
            is_init: false,
        });
        self.env
            .borrow_mut()
            .define(stmt.name.lexeme(), function.to_rc());
        Ok(())
    }

    fn visit_if(&mut self, stmt: &stmt::If) -> Result<()> {
        if let Value::Bool(cond) = *self.evaluate(&stmt.condition)?.borrow() {
            match cond {
                true => self.execute(&stmt.then_branch),
                false => {
                    if let Some(else_branch) = &stmt.else_branch {
                        self.execute(else_branch)
                    } else {
                        Ok(())
                    }
                }
            }
        } else {
            Err(Error::ConditionExpected { stmt: stmt.clone() })
        }
    }

    fn visit_print(&mut self, stmt: &stmt::Print) -> Result<()> {
        let expr = self.evaluate(&stmt.expression)?;
        println!("{}", *expr.borrow());
        Ok(())
    }

    fn visit_return(&mut self, stmt: &stmt::Return) -> Result<()> {
        let value = if let Some(value) = &stmt.value {
            self.evaluate(value)?
        } else {
            Value::Nil.to_rc()
        };
        Err(Error::Return(value))
    }

    fn visit_var(&mut self, stmt: &stmt::Var) -> Result<()> {
        let value = if let Some(init) = &stmt.initializer {
            self.evaluate(init)?
        } else {
            Value::Nil.to_rc()
        };
        self.env.borrow_mut().define(stmt.name.lexeme(), value);
        Ok(())
    }

    fn visit_while(&mut self, stmt: &stmt::While) -> Result<()> {
        while self.evaluate(&stmt.condition)?.borrow().is_truthy() {
            self.execute(&stmt.statement)?;
        }
        Ok(())
    }
}
