use std::fmt::Display;

use crate::{
    ast::{expr::*, stmt::*},
    scanner::token::{Token, TokenType},
    MultiPeekable, Position,
};

#[derive(Debug, Clone)]
pub enum Error {
    General {
        msg: String,
        location: Position,
    },
    InvalidAssignmentTarget {
        target: Expr,
        location: Position,
    },
    UnexpectedTokenType {
        expected: TokenType,
        found: Option<Token>,
    },
}

type Result<T> = std::result::Result<T, Error>;

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::General { msg, location } => {
                write!(f, "{} at {}", msg, location)
            }
            Error::InvalidAssignmentTarget { target, location } => {
                write!(f, "Invalid assignment target at {location}: {target}",)
            }
            Error::UnexpectedTokenType {
                expected,
                found: Some(found),
            } => write!(
                f,
                "expected token type {:?} found {} at {}",
                expected,
                found.lexeme(),
                found.pos()
            ),
            Error::UnexpectedTokenType {
                expected,
                found: None,
            } => write!(f, "expected token type {:?} found EOF", expected,),
        }
    }
}

struct Tokens {
    inner: Vec<Token>,
    idx: usize,
    location: Position,
}

impl Tokens {
    /// Returns last token's location.
    pub fn location(&self) -> Position {
        self.location
    }

    /// Compares the next token type without consuming to the given one.
    pub fn is_type(&self, type_: TokenType) -> bool {
        self.peek().is_some_and(|t| t.get_type() == type_)
    }

    /// Consumes the next token if it matches [`TokenType`].
    ///
    /// If you want to advance the iterator and get [`Token`] instead of [`bool`], use [`Tokens::next_if()`].
    pub fn next_if_type(&mut self, type_: TokenType) -> Option<Token> {
        self.next_if(|t| t.get_type() == type_)
    }

    /// The same as [`Tokens::next_if_type()`] but returns [`bool`].
    pub fn next_if_type_b(&mut self, type_: TokenType) -> bool {
        self.next_if_type(type_).is_some()
    }

    /// Consumes the next token if it matches any of provided [`TokenType`]s.
    ///
    /// If you want to advance the iterator and get [`Token`] instead of [`bool`], use [`Tokens::next_if()`].
    pub fn next_if_type_one_of(&mut self, types: &[TokenType]) -> Option<Token> {
        self.next_if(|lhs| {
            let lhs = lhs.get_type();
            types.iter().any(|rhs| lhs == *rhs)
        })
    }
}

impl Iterator for Tokens {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.get(self.idx).cloned().map(|nx| {
            self.idx += 1;
            self.location = nx.pos();
            nx
        })
    }
}

impl MultiPeekable for Tokens {
    fn peek_nth(&self, idx: usize) -> Option<&Self::Item> {
        let idx = self.idx.checked_add(idx)?;
        self.inner.get(idx)
    }
}

impl From<Vec<Token>> for Tokens {
    fn from(value: Vec<Token>) -> Self {
        Self {
            inner: value,
            idx: 0,
            location: Position::new(1, 1),
        }
    }
}

pub struct Parser {
    tokens: Tokens,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: Tokens::from(tokens),
        }
    }

    pub fn parse(&mut self) -> std::result::Result<Vec<Stmt>, Vec<Error>> {
        let mut statements = vec![];
        let mut errors = vec![];
        while self.tokens.peek().is_some() {
            match self.declaration() {
                Ok(s) => statements.push(s),
                Err(e) => errors.push(e),
            }
        }
        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(statements)
        }
    }

    fn declaration(&mut self) -> Result<Stmt> {
        let res = if self.tokens.next_if_type_b(TokenType::Var) {
            self.var_declaration()
        } else if self.tokens.next_if_type_b(TokenType::Fun) {
            Ok(Stmt::Function(self.fun_declaration()?))
        } else if self.tokens.next_if_type_b(TokenType::Class) {
            self.class_declaration()
        } else {
            self.statement()
        };

        res.map_err(|e| {
            self.synchronize();
            e
        })
    }

    fn class_declaration(&mut self) -> Result<Stmt> {
        let name = self.try_consume(TokenType::Identifier)?;
        self.try_consume(TokenType::LeftBrace)?;
        let mut methods = vec![];
        while !self.tokens.next_if_type_b(TokenType::RightBrace) {
            methods.push(self.fun_declaration()?);
        }
        Ok(Stmt::new_class(name, methods))
    }

    fn fun_declaration(&mut self) -> Result<Function> {
        let name = self.try_consume(TokenType::Identifier)?;
        self.try_consume(TokenType::LeftParen)?;
        let mut params = vec![];
        if !self.tokens.next_if_type_b(TokenType::RightParen) {
            loop {
                params.push(self.try_consume(TokenType::Identifier)?);
                if self.tokens.next_if_type_b(TokenType::RightParen) {
                    break;
                }
                self.try_consume(TokenType::Comma)?;
            }
        }
        self.try_consume(TokenType::LeftBrace)?;
        let body = self.block_statement()?;
        Ok(Function { name, params, body })
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self.try_consume(TokenType::Identifier)?;
        let initializer = if self.tokens.next_if_type_b(TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
        };
        self.try_consume(TokenType::Semicolon)?;
        Ok(Stmt::new_var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.tokens.next_if_type_b(TokenType::Return) {
            self.return_statement()
        } else if self.tokens.next_if_type_b(TokenType::For) {
            self.for_statement()
        } else if self.tokens.next_if_type_b(TokenType::While) {
            self.while_statement()
        } else if self.tokens.next_if_type_b(TokenType::IF) {
            self.if_statement()
        } else if self.tokens.next_if_type_b(TokenType::Print) {
            self.print_statement()
        } else if self.tokens.next_if_type_b(TokenType::LeftBrace) {
            Ok(Stmt::new_block(self.block_statement()?))
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let value = if self.tokens.is_type(TokenType::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        self.try_consume(TokenType::Semicolon)?;
        Ok(Stmt::new_return(value))
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.try_consume(TokenType::LeftParen)?;

        let init = if self.tokens.next_if_type_b(TokenType::Semicolon) {
            None
        } else if self.tokens.next_if_type_b(TokenType::Var) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if self.tokens.is_type(TokenType::Semicolon) {
            Expr::new_bool(true)
        } else {
            self.expression()?
        };
        self.try_consume(TokenType::Semicolon)?;

        let increment = if self.tokens.is_type(TokenType::RightParen) {
            None
        } else {
            Some(self.expression()?)
        };
        self.try_consume(TokenType::RightParen)?;

        let mut body = self.statement()?;

        // Desugaring for -> while
        if let Some(increment) = increment {
            body = Stmt::new_block(vec![body, Stmt::new_expression(increment)])
        }

        body = Stmt::new_while(condition, body.to_box());

        if let Some(init) = init {
            body = Stmt::new_block(vec![init, body]);
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.try_consume(TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.try_consume(TokenType::RightParen)?;
        let body = self.statement()?;
        Ok(Stmt::new_while(condition, body.to_box()))
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.try_consume(TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.try_consume(TokenType::RightParen)?;
        let then_branch = self.statement()?.to_box();
        let else_branch = if self.tokens.next_if_type_b(TokenType::Else) {
            Some(self.statement()?.to_box())
        } else {
            None
        };
        Ok(Stmt::new_if(condition, then_branch, else_branch))
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.try_consume(TokenType::Semicolon)?;
        Ok(Stmt::new_print(expr))
    }

    fn block_statement(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = vec![];
        while self.tokens.peek().is_some() && !self.tokens.is_type(TokenType::RightBrace) {
            statements.push(self.declaration()?);
        }
        self.try_consume(TokenType::RightBrace)?;
        Ok(statements)
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let stmt = Stmt::new_expression(self.expression()?);
        self.try_consume(TokenType::Semicolon)?;
        Ok(stmt)
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.or_expr()?;
        if let Some(equal) = self.tokens.next_if_type(TokenType::Equal) {
            let assign = self.assignment()?;
            if let Expr::Variable(tok) = expr {
                Ok(Expr::new_assign(tok.name, assign.to_box()))
            } else if let Expr::Get(tok) = expr {
                Ok(Expr::new_set(tok.obj, tok.name, assign.to_box()))
            } else {
                Err(Error::InvalidAssignmentTarget {
                    target: expr,
                    location: equal.pos(),
                })
            }
        } else {
            Ok(expr)
        }
    }

    fn or_expr(&mut self) -> Result<Expr> {
        let mut expr = self.and_expr()?;
        while let Some(op) = self.tokens.next_if_type(TokenType::OR) {
            let rhs = self.and_expr()?;
            expr = Expr::new_logical(expr.to_box(), rhs.to_box(), op);
        }
        Ok(expr)
    }

    fn and_expr(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;
        while let Some(op) = self.tokens.next_if_type(TokenType::And) {
            let rhs = self.equality()?;
            expr = Expr::new_logical(expr.to_box(), rhs.to_box(), op);
        }
        Ok(expr)
    }

    /// Generic implementation for binary operations.
    fn binary_op_parser_helper<F: FnMut(&mut Self) -> Result<Expr>>(
        &mut self,
        types: &[TokenType],
        mut descent: F,
    ) -> Result<Expr> {
        let mut expr = descent(self)?;
        while let Some(op) = self.tokens.next_if_type_one_of(types) {
            let rhs = descent(self)?;
            expr = Expr::new_binary(expr.to_box(), rhs.to_box(), op);
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        self.binary_op_parser_helper(
            &[TokenType::BangEqual, TokenType::EqualEqual],
            Self::comparison,
        )
    }

    fn comparison(&mut self) -> Result<Expr> {
        self.binary_op_parser_helper(
            &[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
            Self::term,
        )
    }

    fn term(&mut self) -> Result<Expr> {
        self.binary_op_parser_helper(&[TokenType::Minus, TokenType::Plus], Self::factor)
    }

    fn factor(&mut self) -> Result<Expr> {
        self.binary_op_parser_helper(&[TokenType::Slash, TokenType::Star], Self::unary)
    }

    fn unary(&mut self) -> Result<Expr> {
        if let Some(op) = self
            .tokens
            .next_if_type_one_of(&[TokenType::Bang, TokenType::Minus])
        {
            let expr = self.unary()?;
            Ok(Expr::new_unary(op, expr.to_box()))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;
        loop {
            if self.tokens.next_if_type_b(TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.tokens.next_if_type_b(TokenType::Dot) {
                let name = self.try_consume(TokenType::Identifier)?;
                expr = Expr::new_get(Box::new(expr), name);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
        let mut arguments = vec![];
        if !self.tokens.is_type(TokenType::RightParen) {
            loop {
                arguments.push(self.expression()?);
                if self.tokens.next_if_type(TokenType::Comma).is_none() {
                    break;
                }
            }
        }
        let right_paren = self.try_consume(TokenType::RightParen)?;
        Ok(Expr::new_call(callee.to_box(), arguments, right_paren))
    }

    fn primary(&mut self) -> Result<Expr> {
        if self.tokens.next_if_type_b(TokenType::False) {
            return Ok(Expr::new_bool(false));
        }
        if self.tokens.next_if_type_b(TokenType::True) {
            return Ok(Expr::new_bool(true));
        }
        if self.tokens.next_if_type_b(TokenType::Nil) {
            return Ok(Expr::new_nil());
        }

        if let Some(tok) = self.tokens.next_if_type(TokenType::Number) {
            return Ok(Expr::new_number(tok.lexeme().parse::<f64>().unwrap()));
        }
        if let Some(tok) = self.tokens.next_if_type(TokenType::String) {
            return Ok(Expr::new_string(tok.lexeme().into()));
        }
        if self.tokens.next_if_type_b(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.try_consume(TokenType::RightParen).unwrap();
            return Ok(Expr::new_grouping(expr.to_box()));
        }
        if let Some(tok) = self.tokens.next_if_type(TokenType::Identifier) {
            return Ok(Expr::new_variable(tok));
        }
        Err(Error::General {
            msg: "primary expression".into(),
            location: self.tokens.location(),
        })
    }

    fn synchronize(&mut self) {
        while let Some(typ) = self.tokens.next().map(|t| t.get_type()) {
            if typ == TokenType::Semicolon {
                return;
            }
            if let Some(nt) = self.tokens.peek().map(|t| t.get_type()) {
                match nt {
                    TokenType::Class
                    | TokenType::Fun
                    | TokenType::For
                    | TokenType::IF
                    | TokenType::Print
                    | TokenType::Return
                    | TokenType::Var
                    | TokenType::While => {
                        return;
                    }
                    _ => (),
                }
            }
        }
    }

    fn try_consume(&mut self, type_: TokenType) -> Result<Token> {
        self.tokens
            .next_if(|t| t.get_type() == type_)
            .ok_or(Error::UnexpectedTokenType {
                expected: type_,
                found: self.tokens.peek().cloned(),
            })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::{
        compare_each, pos,
        scanner::{
            token::{Token, TokenType::*},
            Scanner,
        },
    };

    #[test]
    fn variable() {
        let tokens = Scanner::new().scan("var a = 1;").unwrap();
        let stmt = Parser::new(tokens).parse().unwrap();
        let expected = [Stmt::new_var(
            Token::new(Identifier, "a", pos(1, 5)),
            Some(Expr::new_number(1.0)),
        )];
        compare_each(&stmt, &expected);
    }

    #[test]
    fn block() {
        let tokens = Scanner::new().scan("{print a;}").unwrap();
        let stmt = Parser::new(tokens).parse().unwrap();
        let expected = [Stmt::new_block(vec![Stmt::new_print(Expr::new_variable(
            Token::new(Identifier, "a", pos(1, 8)),
        ))])];
        compare_each(&stmt, &expected);
    }

    #[test]
    fn while_loop() {
        let tokens = Scanner::new().scan("while (a > 2) {}").unwrap();
        let stmt = Parser::new(tokens).parse().unwrap();
        let expected = [Stmt::new_while(
            Expr::new_binary(
                Expr::new_variable(Token::new(Identifier, "a", pos(1, 8))).into(),
                Expr::new_number(2.0).into(),
                Token::new(Greater, ">", pos(1, 10)),
            ),
            Stmt::new_block(vec![]).into(),
        )];
        compare_each(&stmt, &expected);
    }

    #[test]
    fn function_call() {
        let tokens = Scanner::new().scan("var a = call(1, 2);").unwrap();
        let stmt = Parser::new(tokens).parse().unwrap();
        let expected = [Stmt::new_var(
            Token::new(Identifier, "a", pos(1, 5)),
            Expr::new_call(
                Expr::new_variable(Token::new(Identifier, "call", pos(1, 9))).into(),
                vec![Expr::new_number(1.0), Expr::new_number(2.0)],
                Token::new(RightParen, ')', pos(1, 18)),
            )
            .into(),
        )];
        compare_each(&stmt, &expected);
    }

    #[test]
    fn function_declaration() {
        let tokens = Scanner::new().scan("fun mul(a, b) {}").unwrap();
        let stmt = Parser::new(tokens).parse().unwrap();
        let expected = [Stmt::new_function(
            Token::new(Identifier, "mul", pos(1, 5)),
            vec![
                Token::new(Identifier, "a", pos(1, 9)),
                Token::new(Identifier, "b", pos(1, 12)),
            ],
            vec![],
        )];

        compare_each(&stmt, &expected);
    }

    #[test]
    fn class_declaration() {
        let tokens = Scanner::new()
            .scan("class Random {method() {return;}}")
            .unwrap();
        let stmt = Parser::new(tokens).parse().unwrap();
        let expected = [Stmt::new_class(
            Token::new(Identifier, "Random", pos(1, 7)),
            vec![Function {
                name: Token::new(Identifier, "method", pos(1, 15)),
                params: vec![],
                body: vec![Stmt::new_return(None)],
            }],
        )];

        compare_each(&stmt, &expected);
    }
}
