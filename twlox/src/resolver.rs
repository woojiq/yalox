use std::{
    collections::{HashMap, VecDeque},
    fmt,
};

use crate::{
    ast::{
        expr::{ExprVisitor, *},
        stmt::{StmtVisitor, *},
    },
    interpreter::Interpreter,
    scanner::token::Token,
};

#[derive(Debug, Clone)]
pub enum Error {
    ReadInInit { name: Token },
    AlreadyExists { name: Token },
    ReturnNotInFunc,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ReadInInit { name } => write!(
                f,
                "Value {} is referenced in own initializer at {}",
                name.lexeme(),
                name.pos()
            ),
            Error::AlreadyExists { name } => write!(
                f,
                "Variable with the {} name at {} already defined",
                name.lexeme(),
                name.pos()
            ),
            Error::ReturnNotInFunc => write!(f, "Return is disallowed in global scope."),
        }
    }
}

type RResult = std::result::Result<(), Vec<Error>>;

#[derive(Copy, Clone, Debug)]
pub enum FunctionType {
    Function,
    None,
}

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: VecDeque<HashMap<String, bool>>,
    current_func: FunctionType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: VecDeque::default(),
            current_func: FunctionType::None,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push_back(HashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop_back();
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> RResult {
        stmt.accept(self)
    }

    pub fn resolve_stmts(&mut self, stmts: &[Stmt]) -> RResult {
        let mut errors = vec![];
        for stmt in stmts {
            if let Err(mut err) = self.resolve_stmt(stmt) {
                errors.append(&mut err);
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn declare(&mut self, name: &Token) -> Result<(), Error> {
        let Some(scope) = self.scopes.back_mut() else {
            return Ok(());
        };
        if scope.get(name.lexeme()).is_some() {
            return Err(Error::AlreadyExists { name: name.clone() });
        }
        scope.insert(name.lexeme().to_string(), false);
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        let Some(scope) = self.scopes.back_mut() else {
            return;
        };
        scope.insert(name.lexeme().to_string(), true);
    }

    fn resolve_local(&mut self, name: &Token) {
        for (idx, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name.lexeme()) {
                self.interpreter.resolve(name.pos(), idx);
                return;
            }
        }
    }

    fn resolve_func(&mut self, func: &Function, typ: FunctionType) -> RResult {
        let save_type = typ;
        self.current_func = typ;
        let mut errors = vec![];
        self.begin_scope();
        for param in &func.params {
            if let Err(err) = self.declare(param) {
                errors.push(err);
            }
            self.define(param);
        }
        self.resolve_stmts(&func.body)?;
        self.end_scope();
        self.current_func = save_type;
        match errors.is_empty() {
            true => Ok(()),
            false => Err(errors),
        }
    }
}

impl<'a> ExprVisitor<RResult> for Resolver<'a> {
    fn visit_literal(&mut self, _expr: &Literal) -> RResult {
        Ok(())
    }

    fn visit_call(&mut self, expr: &Call) -> RResult {
        expr.callee.accept(self)?;
        for arg in &expr.arguments {
            arg.accept(self)?;
        }
        Ok(())
    }

    fn visit_unary(&mut self, expr: &Unary) -> RResult {
        expr.inner.accept(self)
    }

    fn visit_logical(&mut self, expr: &Logical) -> RResult {
        expr.left.accept(self)?;
        expr.right.accept(self)?;
        Ok(())
    }

    fn visit_grouping(&mut self, expr: &Grouping) -> RResult {
        expr.inner.accept(self)
    }

    fn visit_binary(&mut self, expr: &Binary) -> RResult {
        expr.left.accept(self)?;
        expr.right.accept(self)?;
        Ok(())
    }

    fn visit_assign(&mut self, expr: &Assign) -> RResult {
        expr.value.accept(self)?;
        expr.value.accept(self)?;
        Ok(())
    }

    fn visit_variable(&mut self, expr: &Variable) -> RResult {
        if self
            .scopes
            .back()
            .is_some_and(|s| s.get(expr.name.lexeme()).is_some_and(|b| !*b))
        {
            Err(vec![Error::ReadInInit {
                name: expr.name.clone(),
            }])
        } else {
            self.resolve_local(&expr.name);
            Ok(())
        }
    }

    fn visit_get(&mut self, expr: &Get) -> RResult {
        expr.obj.accept(self)
    }
}

impl<'a> StmtVisitor<RResult> for Resolver<'a> {
    fn visit_block(&mut self, stmt: &Block) -> RResult {
        self.begin_scope();
        self.resolve_stmts(&stmt.statements)?;
        self.end_scope();
        Ok(())
    }

    fn visit_class(&mut self, stmt: &Class) -> RResult {
        if let Err(err) = self.declare(&stmt.name) {
            return Err(vec![err]);
        }
        self.define(&stmt.name);
        Ok(())
    }

    fn visit_expression(&mut self, stmt: &Expression) -> RResult {
        stmt.expression.accept(self)
    }

    fn visit_function(&mut self, stmt: &Function) -> RResult {
        let mut errors = vec![];
        if let Err(err) = self.declare(&stmt.name) {
            errors.push(err);
        }
        self.define(&stmt.name);
        if let Err(mut errs) = self.resolve_func(stmt, FunctionType::Function) {
            errors.append(&mut errs);
        }
        match errors.is_empty() {
            true => Ok(()),
            false => Err(errors),
        }
    }

    fn visit_if(&mut self, stmt: &If) -> RResult {
        stmt.condition.accept(self)?;
        self.resolve_stmt(&stmt.then_branch)?;
        if let Some(br) = &stmt.else_branch {
            self.resolve_stmt(br)?;
        }
        Ok(())
    }

    fn visit_print(&mut self, stmt: &Print) -> RResult {
        stmt.expression.accept(self)
    }

    fn visit_return(&mut self, stmt: &Return) -> RResult {
        if let FunctionType::None = self.current_func {
            return Err(vec![Error::ReturnNotInFunc]);
        }
        if let Some(ret) = &stmt.value {
            ret.accept(self)?;
        }
        Ok(())
    }

    fn visit_var(&mut self, stmt: &Var) -> RResult {
        let mut errors = vec![];
        if let Err(err) = self.declare(&stmt.name) {
            errors.push(err);
        }
        if let Some(init) = &stmt.initializer {
            if let Err(mut err) = init.accept(self) {
                errors.append(&mut err);
            }
        }
        self.define(&stmt.name);
        match errors.is_empty() {
            true => Ok(()),
            false => Err(errors),
        }
    }

    fn visit_while(&mut self, stmt: &While) -> RResult {
        stmt.condition.accept(self)?;
        self.resolve_stmt(&stmt.statement)
    }
}
