use std::collections::{HashMap, VecDeque};

use crate::{
    ast::{expr::*, stmt::*},
    scanner::token::Token,
};

#[derive(thiserror::Error)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    #[error("variable '{}' is referenced in own initializer at {}", .name.lexeme(), .name.pos())]
    ReadInInit { name: Token },
    #[error("variable '{}' name already defined at {}", .name.lexeme(), .name.pos())]
    AlreadyExists { name: Token },
    #[error("return is disallowed in global scope at {}", .keyword.pos())]
    GlobalReturn { keyword: Token },
    #[error("\"return\" with a value in an initializer is disallowed at {}", .keyword.pos())]
    ReturnInInitializer { keyword: Token },
    #[error("\"this\" keyword is allowed only in classes at {}", .keyword.pos())]
    ThisNotInClass { keyword: Token },
}

type Result<T = (), E = Vec<Error>> = std::result::Result<T, E>;

#[derive(Copy, Clone, Debug)]
pub enum FunctionType {
    Function,
    Initializer,
    Method,
    None,
}

#[derive(Copy, Clone, Debug)]
pub enum ClassType {
    Class,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ResolvedEntry {
    pub pos: crate::Position,
    pub depth: usize,
}

impl ResolvedEntry {
    pub fn new(pos: crate::Position, depth: usize) -> Self {
        Self { pos, depth }
    }
}

pub struct Resolver<'a> {
    resolve_cb: &'a mut dyn FnMut(ResolvedEntry),
    scopes: VecDeque<HashMap<String, bool>>,
    errors: Vec<Error>,
    current_func: FunctionType,
    current_class: ClassType,
}

impl<'a> Resolver<'a> {
    pub fn new(resolve_cb: &'a mut dyn FnMut(ResolvedEntry)) -> Self {
        Self {
            resolve_cb,
            scopes: VecDeque::default(),
            errors: Vec::default(),
            current_func: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push_back(HashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop_back();
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) {
        stmt.accept(self);
    }

    pub fn resolve(mut self, stmts: &[Stmt]) -> Result {
        self.resolve_stmts(stmts);
        match self.errors.is_empty() {
            true => Ok(()),
            false => Err(self.errors),
        }
    }

    fn resolve_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.resolve_stmt(stmt);
        }
    }

    fn declare(&mut self, name: &Token) {
        // We do not want to track global variables.
        let Some(scope) = self.scopes.back_mut() else {
            return;
        };
        if scope.get(name.lexeme()).is_some() {
            self.errors.push(Error::AlreadyExists { name: name.clone() });
            return;
        }
        scope.insert(name.lexeme().to_string(), false);
    }

    fn define(&mut self, name: &Token) {
        // We do not want to track global variables.
        let Some(scope) = self.scopes.back_mut() else {
            return;
        };
        scope.insert(name.lexeme().to_string(), true);
    }

    fn resolve_local(&mut self, name: &Token) {
        // If a variable was found in the current scope, we pass 0.
        // If found in the enclosing scope, then 1, and so on.
        for (idx, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name.lexeme()) {
                (self.resolve_cb)(ResolvedEntry::new(name.pos(), idx));
                return;
            }
        }
    }

    fn resolve_func(&mut self, func: &Function, typ: FunctionType) {
        let save_type = std::mem::replace(&mut self.current_func, typ);
        self.begin_scope();
        for param in &func.params {
            self.declare(param);
            self.define(param);
        }
        self.resolve_stmts(&func.body);
        self.end_scope();
        self.current_func = save_type;
    }
}

impl<'a> ExprVisitor<()> for Resolver<'a> {
    fn visit_literal(&mut self, _expr: &Literal) {}

    fn visit_call(&mut self, expr: &Call) {
        expr.callee.accept(self);
        for arg in &expr.arguments {
            arg.accept(self);
        }
    }

    fn visit_unary(&mut self, expr: &Unary) {
        expr.inner.accept(self)
    }

    fn visit_logical(&mut self, expr: &Logical) {
        expr.left.accept(self);
        expr.right.accept(self);
    }

    fn visit_grouping(&mut self, expr: &Grouping) {
        expr.inner.accept(self)
    }

    fn visit_binary(&mut self, expr: &Binary) {
        expr.left.accept(self);
        expr.right.accept(self);
    }

    fn visit_assign(&mut self, expr: &Assign) {
        expr.value.accept(self);
        self.resolve_local(&expr.name);
    }

    fn visit_variable(&mut self, expr: &Variable) {
        if self.scopes.back().is_some_and(|s| s.get(expr.name.lexeme()).is_some_and(|b| !*b)) {
            self.errors.push(Error::ReadInInit { name: expr.name.clone() });
        } else {
            self.resolve_local(&expr.name);
        }
    }

    fn visit_get(&mut self, expr: &Get) {
        expr.obj.accept(self)
    }

    fn visit_set(&mut self, expr: &Set) {
        expr.value.accept(self);
        expr.obj.accept(self);
    }

    fn visit_this(&mut self, expr: &This) {
        match self.current_class {
            ClassType::Class => {
                self.resolve_local(&expr.keyword);
            }
            ClassType::None => {
                self.errors.push(Error::ThisNotInClass { keyword: expr.keyword.clone() })
            }
        }
    }
}

impl<'a> StmtVisitor<()> for Resolver<'a> {
    fn visit_block(&mut self, stmt: &Block) {
        self.begin_scope();
        self.resolve_stmts(&stmt.statements);
        self.end_scope();
    }

    fn visit_class(&mut self, stmt: &Class) {
        let enclosing_class = std::mem::replace(&mut self.current_class, ClassType::Class);
        self.declare(&stmt.name);
        self.define(&stmt.name);
        self.begin_scope();
        // TODO: replace "init" and "this" with constants or something.
        self.scopes.back_mut().unwrap().insert("this".to_string(), true);
        for method in &stmt.methods {
            let typ = if method.name.lexeme() == "init" {
                FunctionType::Initializer
            } else {
                FunctionType::Method
            };
            self.resolve_func(method, typ);
        }
        self.end_scope();
        self.current_class = enclosing_class;
    }

    fn visit_expression(&mut self, stmt: &Expression) {
        stmt.expression.accept(self);
    }

    fn visit_function(&mut self, stmt: &Function) {
        self.declare(&stmt.name);
        self.define(&stmt.name);
        self.resolve_func(stmt, FunctionType::Function);
    }

    fn visit_if(&mut self, stmt: &If) {
        stmt.condition.accept(self);
        self.resolve_stmt(&stmt.then_branch);
        if let Some(br) = &stmt.else_branch {
            self.resolve_stmt(br);
        }
    }

    fn visit_print(&mut self, stmt: &Print) {
        stmt.expression.accept(self)
    }

    fn visit_return(&mut self, stmt: &Return) {
        if let FunctionType::None = self.current_func {
            self.errors.push(Error::GlobalReturn { keyword: stmt.keyword.clone() });
            return;
        }
        if let Some(ret) = &stmt.value {
            if let FunctionType::Initializer = self.current_func {
                self.errors.push(Error::ReturnInInitializer { keyword: stmt.keyword.clone() });
                return;
            }
            ret.accept(self);
        }
    }

    fn visit_var(&mut self, stmt: &Var) {
        self.declare(&stmt.name);
        if let Some(init) = &stmt.initializer {
            init.accept(self);
        }
        self.define(&stmt.name);
    }

    fn visit_while(&mut self, stmt: &While) {
        stmt.condition.accept(self);
        self.resolve_stmt(&stmt.statement);
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use super::*;
    use crate::{
        parser::Parser,
        pos,
        scanner::{token::TokenType, Scanner},
    };

    struct ResolverCbMock {
        expected: HashSet<ResolvedEntry>,
    }

    impl ResolverCbMock {
        fn new(expected: &[ResolvedEntry]) -> Self {
            Self { expected: HashSet::from_iter(expected.iter().cloned()) }
        }

        fn verify(&mut self, got: ResolvedEntry) {
            assert!(self.expected.remove(&got), "resolved entry {:?} is not expected", got);
        }
    }

    // I love RAII.
    impl Drop for ResolverCbMock {
        fn drop(&mut self) {
            if !std::thread::panicking() {
                assert!(
                    self.expected.is_empty(),
                    "not all expected entries were resolved: {:#?}",
                    self.expected
                );
            }
        }
    }

    #[test]
    fn function_with_while_loop() {
        // Currently we cannot test "for" loop because it desugars to "while" during
        // parsing.
        let src = r#"// first line
var a = 0;
var cnt = 3;
fun add(a) {
    var copy_cnt = cnt;
    while (copy_cnt > 0) {
        a = a + 1;
        copy_cnt = --copy_cnt - 1;
    }
    a = a + 1;
}
add(a); add(a);
"#;
        let mut mock = ResolverCbMock::new(&[
            ResolvedEntry::new(pos(6, 12), 0),
            ResolvedEntry::new(pos(7, 9), 1),
            ResolvedEntry::new(pos(7, 13), 1),
            ResolvedEntry::new(pos(8, 9), 1),
            ResolvedEntry::new(pos(8, 22), 1),
            ResolvedEntry::new(pos(10, 5), 0),
            ResolvedEntry::new(pos(10, 9), 0),
        ]);
        let mut cb = |entry| mock.verify(entry);

        let tokens = Scanner::new().scan(src).unwrap();
        let stmts = Parser::new(tokens).parse().unwrap();
        Resolver::new(&mut cb).resolve(&stmts).unwrap();
    }

    #[test]
    fn compile_time_keywords_errors() {
        let test = "\
fun main() {print this;}
var a = 1; return a;
class TestClass {init() {return 1;} }
        ";
        let errors = [
            Error::ThisNotInClass { keyword: Token::new(TokenType::This, "this", pos(1, 19)) },
            Error::GlobalReturn { keyword: Token::new(TokenType::Return, "return", pos(2, 12)) },
            Error::ReturnInInitializer {
                keyword: Token::new(TokenType::Return, "return", pos(3, 26)),
            },
        ];
        // TODO: Combine Scanner + Parser in one helper function and reuse everywhere.
        let tokens = Scanner::new().scan(test).unwrap();
        let stmts = Parser::new(tokens).parse().unwrap();
        let mut cb = |_| {};
        let res = Resolver::new(&mut cb).resolve(&stmts).unwrap_err();
        assert_eq!(res.as_slice(), &errors);
    }

    #[test]
    fn this_keyword_in_method() {
        let src = r#"// first line
class main {
    init() {
        this.a = 0;
    }
}
{var q = main();}
"#;
        let mut mock = ResolverCbMock::new(&[ResolvedEntry::new(pos(4, 9), 1)]);
        let mut cb = |entry| mock.verify(entry);

        let tokens = Scanner::new().scan(src).unwrap();
        let stmts = Parser::new(tokens).parse().unwrap();
        Resolver::new(&mut cb).resolve(&stmts).unwrap();
    }
}
