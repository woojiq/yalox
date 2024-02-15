pub mod environment;
pub mod value;

use crate::{
    ast::{
        expr::{Expr, ExprVisitor},
        stmt::{Stmt, StmtVisitor},
    },
    scanner::token::{Token, TokenType},
    Position,
};

use core::fmt;
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::ast::*;

use self::{
    environment::Environment,
    value::{Callable, Function, NativeFunction, Value, ValueOpError},
};

#[derive(Debug, Clone)]
pub enum Error {
    BinaryOp { msg: String, op: Token },
    UnaryOp { msg: String, op: Token },
    UndefinedVariable { tok: Token },
    Arity { callee: Value },
    ConditionExpected { stmt: stmt::If },
    PropertyNotOnObj { property: Token },

    // === Not exactly Errors === //
    Return(Value),
}

type Result<T> = std::result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::BinaryOp { msg, op } => write!(f, "{} at {}", msg, op.pos()),
            Error::UnaryOp { msg, op } => write!(f, "{} at {}", msg, op.pos()),
            Error::UndefinedVariable { tok } => {
                write!(f, "undefined variable {} at {}", tok.lexeme(), tok.pos())
            }
            Error::Arity { callee } => write!(f, "inappropriate arity {}", callee),
            Error::ConditionExpected { stmt } => {
                write!(f, "expected boolean in condition: {}", stmt.condition)
            }
            Error::Return(_) => unreachable!(),
            Error::PropertyNotOnObj { property: prop } => {
                write!(f, "only instances have properties at {}", prop.pos())
            }
        }
    }
}

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
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
            }),
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

    fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
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
        res?;
        Ok(())
    }

    pub fn env(&self) -> Ref<'_, Environment> {
        self.env.borrow()
    }

    pub fn resolve(&mut self, pos: Position, depth: usize) {
        self.locals.insert(pos, depth);
    }

    fn lookup_variable(&self, token: Token) -> Result<Value> {
        if let Some(depth) = self.locals.get(&token.pos()) {
            self.env.borrow().get_at(token.lexeme(), *depth)
        } else {
            self.globals.borrow().get(token.lexeme())
        }
        .ok_or(Error::UndefinedVariable { tok: token })
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl ExprVisitor<Result<Value>> for Interpreter {
    fn visit_binary(&mut self, expr: &expr::Binary) -> Result<Value> {
        let lhs = self.evaluate(&expr.left)?;
        let rhs = self.evaluate(&expr.right)?;
        let res: std::result::Result<Value, ValueOpError> = {
            let (lhs, rhs) = (lhs.clone(), rhs.clone());
            match expr.op.get_type() {
                TokenType::Minus => lhs.sub(rhs),
                TokenType::Plus => lhs.add(rhs),
                TokenType::Slash => lhs.div(rhs),
                TokenType::Star => lhs.mul(rhs),
                TokenType::BangEqual => Ok(lhs.ne(&rhs).into()),
                TokenType::EqualEqual => Ok(lhs.eq(&rhs).into()),
                TokenType::Greater => Ok((lhs > rhs).into()),
                TokenType::GreaterEqual => Ok((lhs >= rhs).into()),
                TokenType::Less => Ok((lhs < rhs).into()),
                TokenType::LessEqual => Ok((lhs <= rhs).into()),
                TokenType::False => Ok(false.into()),
                TokenType::True => Ok(true.into()),
                _ => unreachable!(),
            }
        };
        res.map_err(|e| Error::BinaryOp {
            msg: e.to_string(),
            op: expr.op.clone(),
        })
    }

    fn visit_grouping(&mut self, expr: &expr::Grouping) -> Result<Value> {
        self.evaluate(&expr.inner)
    }

    fn visit_literal(&mut self, expr: &expr::Literal) -> Result<Value> {
        Ok(match expr {
            expr::Literal::Number(n) => Value::Num(*n),
            expr::Literal::String(s) => Value::Str(s.to_string()),
            expr::Literal::Bool(b) => Value::Bool(*b),
            expr::Literal::Nil => Value::Nil,
        })
    }

    fn visit_unary(&mut self, expr: &expr::Unary) -> Result<Value> {
        let val = self.evaluate(&expr.inner)?;
        let res = {
            let val = val.clone();
            match expr.op.get_type() {
                TokenType::Minus => -val,
                TokenType::Bang => !val,
                _ => unreachable!(),
            }
        };

        res.map_err(|e| Error::UnaryOp {
            msg: e.to_string(),
            op: expr.op.clone(),
        })
    }

    fn visit_variable(&mut self, expr: &expr::Variable) -> Result<Value> {
        self.lookup_variable(expr.name.clone())
    }

    fn visit_assign(&mut self, expr: &expr::Assign) -> Result<Value> {
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
            true => Ok(value),
            false => Err(Error::UndefinedVariable {
                tok: expr.name.clone(),
            }),
        }
    }

    fn visit_logical(&mut self, expr: &expr::Logical) -> Result<Value> {
        let lhs: bool = self.evaluate(&expr.left)?.into();
        match expr.op.get_type() {
            TokenType::And => Ok(if lhs {
                Value::Bool(self.evaluate(&expr.right)?.into())
            } else {
                Value::Bool(false)
            }),
            TokenType::OR => Ok(if !lhs {
                Value::Bool(self.evaluate(&expr.right)?.into())
            } else {
                Value::Bool(true)
            }),
            _ => unreachable!(),
        }
    }

    fn visit_call(&mut self, expr: &expr::Call) -> Result<Value> {
        let callee = self.evaluate(&expr.callee)?;
        let callable: Box<dyn Callable> = match callee.clone() {
            Value::Function(f) => Box::new(f),
            Value::NativeFunction(f) => Box::new(f),
            _ => unreachable!(),
        };

        let args = expr
            .arguments
            .iter()
            .map(|a| self.evaluate(a))
            .collect::<Result<Vec<_>>>()?;

        if args.len() != callable.arity() {
            return Err(Error::Arity { callee });
        }
        callable.call(self, args)
    }

    fn visit_get(&mut self, expr: &expr::Get) -> Result<Value> {
        todo!();
        let obj = self.evaluate(&expr.obj)?;
        if let Value::Instance(val) = &obj {
            return Ok(val.get(&expr.name));
        }
        Err(Error::PropertyNotOnObj {
            property: expr.name.clone(),
        })
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
        self.env.borrow_mut().define(stmt.name.lexeme(), Value::Nil);
        let class = Value::Class(stmt.clone());
        self.env.borrow_mut().assign(stmt.name.lexeme(), class);
        Ok(())
    }

    fn visit_expression(&mut self, stmt: &stmt::Expression) -> Result<()> {
        self.evaluate(&stmt.expression).map(|_| ())
    }

    fn visit_function(&mut self, stmt: &stmt::Function) -> Result<()> {
        let function = Value::Function(Function {
            declaration: stmt.clone(),
            closure: self.env.clone(),
        });
        self.env.borrow_mut().define(stmt.name.lexeme(), function);
        Ok(())
    }

    fn visit_if(&mut self, stmt: &stmt::If) -> Result<()> {
        if let Value::Bool(cond) = self.evaluate(&stmt.condition)? {
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
        println!("{}", expr);
        Ok(())
    }

    fn visit_return(&mut self, stmt: &stmt::Return) -> Result<()> {
        let value = if let Some(value) = &stmt.value {
            self.evaluate(value)?
        } else {
            Value::Nil
        };
        Err(Error::Return(value))
    }

    fn visit_var(&mut self, stmt: &stmt::Var) -> Result<()> {
        let value = if let Some(init) = &stmt.initializer {
            self.evaluate(init)?
        } else {
            Value::Nil
        };
        self.env.borrow_mut().define(stmt.name.lexeme(), value);
        Ok(())
    }

    fn visit_while(&mut self, stmt: &stmt::While) -> Result<()> {
        while bool::from(self.evaluate(&stmt.condition)?) {
            self.execute(&stmt.statement)?;
        }
        Ok(())
    }
}
