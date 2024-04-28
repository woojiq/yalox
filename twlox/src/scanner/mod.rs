pub mod token;

use std::fmt;

use token::{Token, TokenType};

use crate::{MultiPeekable, Position};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Error {
    UnterminatedString(Position),
    UnexpectedChar { position: Position, character: char },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::UnterminatedString(start) =>
                    format!("Unterminated string started from {}", start),
                Error::UnexpectedChar { position, character } =>
                    format!("Unexpected character '{}' at {}", character, position),
            }
        )
    }
}

struct Chars {
    inner: Vec<char>,
    head: usize,
    tail: usize,
    pos: Position,
}

impl Chars {
    pub fn new(source: &str, pos: Position) -> Self {
        Self { inner: source.chars().collect(), head: 0, tail: 0, pos }
    }

    /// Creates substring from the tail to the head not inclusively.
    pub fn substr_inside(&self) -> String {
        assert!(self.head - 1 > self.tail);
        self.inner[self.tail + 1..self.head - 1].iter().collect::<String>()
    }

    /// Creates substring from the tail to the head inclusively.
    pub fn substr_around(&self) -> String {
        self.inner[self.tail..self.head].iter().collect::<String>()
    }

    /// Synchronize the tail with the head.
    pub fn sync(&mut self) {
        self.tail = self.head;
    }

    /// Processes a character and determines the position in the document after
    /// this character.
    fn eat(&mut self, val: char) {
        if val == '\n' {
            self.pos = Position::new(self.pos.row + 1, 0);
        }
        self.pos.col += 1;
    }
}

impl Iterator for Chars {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.get(self.head).cloned().map(|val| {
            self.eat(val);
            self.head += 1;
            val
        })
    }
}

impl MultiPeekable for Chars {
    fn peek_nth(&self, idx: usize) -> Option<&Self::Item> {
        let pos = self.head.checked_add(idx)?;
        self.inner.get(pos)
    }
}

/// A stateful scanner that produces a list of tokens from a text.
///
/// This scanner stores its position, and subsequent calls to [`self.scan()`]
/// will reuse and change this position. In general this is fine even for REPLs,
/// but if you want to reset the position use [`self.reset()`].
pub struct Scanner {
    pos: Position,
}

impl Scanner {
    pub fn new() -> Self {
        Self { pos: Position::new(1, 1) }
    }

    /// Resets scanner position state.
    pub fn reset(&mut self) {
        self.pos = Position::new(1, 1);
    }

    /// Creates list of tokens or errors from the text.
    pub fn scan(&mut self, source: &str) -> Result<Vec<Token>, Vec<Error>> {
        let mut chars = Chars::new(source, self.pos);
        let mut tokens = vec![];
        let mut errors = vec![];

        while let Some(ch) = chars.next() {
            match self.scan_token(ch, &mut chars) {
                Ok(Some(t)) => tokens.push(t),
                Ok(None) => (),
                Err(e) => errors.push(e),
            }
            self.pos = chars.pos;
            chars.sync();
        }

        match errors.is_empty() {
            true => Ok(tokens),
            false => Err(errors),
        }
    }

    fn scan_token(&mut self, ch: char, chars: &mut Chars) -> Result<Option<Token>, Error> {
        macro_rules! token {
            ($type:ident) => {
                Token::new(TokenType::$type, ch, self.pos)
            };
            ($type:ident, $lexeme:expr) => {
                Token::new(TokenType::$type, $lexeme, self.pos)
            };
            (next $second:literal then $then:ident else $else:ident) => {
                if chars.next_if_eq(&$second).is_some() {
                    token!($then, format!("{}{}", ch, $second))
                } else {
                    token!($else)
                }
            };
        }

        let token = match ch {
            '(' => token!(LeftParen),
            ')' => token!(RightParen),
            '{' => token!(LeftBrace),
            '}' => token!(RightBrace),
            ',' => token!(Comma),
            '.' => token!(Dot),
            '-' => token!(Minus),
            '+' => token!(Plus),
            ';' => token!(Semicolon),
            '*' => token!(Star),
            '!' => token!(next '=' then BangEqual else Bang),
            '=' => token!(next '=' then EqualEqual else Equal),
            '<' => token!(next '=' then LessEqual else Less),
            '>' => token!(next '=' then GreaterEqual else Greater),
            '/' => {
                if chars.next_if_eq(&'/').is_some() {
                    while chars.next_if_ne(&'\n').is_some() {}
                    return Ok(None);
                } else {
                    token!(Slash)
                }
            }
            ' ' | '\r' | '\t' | '\n' => {
                return Ok(None);
            }
            '"' => {
                return self.scan_string(chars).map(Some);
            }
            d if d.is_ascii_digit() => self.scan_floating_num(chars),
            ch if ch.is_ascii_alphabetic() => self.scan_word(chars),
            unexpected => {
                return Err(Error::UnexpectedChar { position: self.pos, character: unexpected });
            }
        };

        Ok(Some(token))
    }

    fn scan_string(&self, chars: &mut Chars) -> Result<Token, Error> {
        while chars.next_if_ne(&'"').is_some() {}

        if chars.peek().is_none() {
            Err(Error::UnterminatedString(self.pos))
        } else {
            chars.next();
            let lexeme = chars.substr_inside();
            Ok(Token::new(TokenType::String, lexeme, self.pos))
        }
    }

    fn scan_floating_num(&self, chars: &mut Chars) -> Token {
        while chars.next_if(|ch| ch.is_ascii_digit()).is_some() {}

        match (chars.peek_nth(0), chars.peek_nth(1)) {
            (Some('.'), Some(c)) if c.is_ascii_digit() => {
                chars.next();
                while chars.next_if(|ch| ch.is_ascii_digit()).is_some() {}
            }
            _ => (),
        }

        let lexeme = chars.substr_around();
        Token::new(TokenType::Number, lexeme, self.pos)
    }

    fn scan_word(&self, chars: &mut Chars) -> Token {
        while chars.next_if(|ch| ch.is_alphanumeric() || ch == &'_').is_some() {}

        let lexeme = chars.substr_around();
        let type_ = token::get_keyword(&lexeme).unwrap_or(TokenType::Identifier);
        Token::new(type_, lexeme, self.pos)
    }
}

impl Default for Scanner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{compare_each, pos};

    #[test]
    fn numbers() {
        let mut scanner = Scanner::default();
        let tests = [
            ("123.456", None),
            ("123", None),
            ("123.float", Some("123")),
            ("0", None),
            ("012.12", None),
            ("98.76..12", Some("98.76")),
        ];
        for (test, expected) in tests {
            scanner.reset();
            let tokens = scanner.scan(test).unwrap();
            assert_eq!(
                tokens[0].get_type(),
                TokenType::Number,
                "{} should be determined as a number",
                test
            );
            let expected = expected.unwrap_or(test);
            assert_eq!(tokens[0].lexeme(), expected, "'{}' part of '{}' is number", expected, test);
        }
    }

    #[test]
    fn number_of_scanned_tokens() {
        let mut scanner = Scanner::default();
        let test_cases = [(
            "* var name=12.3 float.num\n//this doesn't count\\\\ \n (1+2)/\"string\"\n1,2,3",
            20,
        )];
        for (test, expected_tokens) in test_cases {
            scanner.reset();
            let tokens = scanner.scan(test).unwrap();
            assert_eq!(tokens.len(), expected_tokens, "input:\n{}\ntokens: {:?}", test, tokens);
        }
    }

    #[test]
    fn single_line_correct() {
        let tokens = Scanner::default().scan("var a = 1;").unwrap();
        let expected = [
            Token::new(TokenType::Var, "var", pos(1, 1)),
            Token::new(TokenType::Identifier, "a", pos(1, 5)),
            Token::new(TokenType::Equal, "=", pos(1, 7)),
            Token::new(TokenType::Number, "1", pos(1, 9)),
            Token::new(TokenType::Semicolon, ";", pos(1, 10)),
        ];
        compare_each(&tokens, &expected)
    }

    #[test]
    fn unterminated_string() {
        let mut scanner = Scanner::default();
        let test_case = [(r#"""#, Position::new(1, 1)), ("123\"456\n789", Position::new(1, 4))];
        for (test, expected_pos) in test_case {
            scanner.reset();
            let errors = scanner.scan(test).err().unwrap();
            assert_eq!(errors.len(), 1);
            assert_eq!(
                errors[0],
                Error::UnterminatedString(expected_pos),
                "expected unterminated string error on {} in input:\n{}",
                expected_pos,
                test
            );
        }
    }

    #[test]
    fn math_lexemes() {
        let tokens = Scanner::default().scan("(+*/{})").unwrap();
        let expected = [
            Token::new(TokenType::LeftParen, '(', pos(1, 1)),
            Token::new(TokenType::Plus, '+', pos(1, 2)),
            Token::new(TokenType::Star, '*', pos(1, 3)),
            Token::new(TokenType::Slash, '/', pos(1, 4)),
            Token::new(TokenType::LeftBrace, '{', pos(1, 5)),
            Token::new(TokenType::RightBrace, '}', pos(1, 6)),
            Token::new(TokenType::RightParen, ')', pos(1, 7)),
        ];
        compare_each(&tokens, &expected);
    }

    #[test]
    fn operation_lexemes() {
        let tokens = Scanner::default().scan("+= == ! =/ <=").unwrap();
        let expected = [
            Token::new(TokenType::Plus, '+', pos(1, 1)),
            Token::new(TokenType::Equal, '=', pos(1, 2)),
            Token::new(TokenType::EqualEqual, "==", pos(1, 4)),
            Token::new(TokenType::Bang, '!', pos(1, 7)),
            Token::new(TokenType::Equal, '=', pos(1, 9)),
            Token::new(TokenType::Slash, '/', pos(1, 10)),
            Token::new(TokenType::LessEqual, "<=", pos(1, 12)),
        ];
        compare_each(&tokens, &expected);
    }

    #[test]
    fn unexpected_char() {
        let errors = Scanner::default().scan("[]\n\n  ^\n 😃").err().unwrap();
        let expected = [
            Error::UnexpectedChar { position: pos(1, 1), character: '[' },
            Error::UnexpectedChar { position: pos(1, 2), character: ']' },
            Error::UnexpectedChar { position: pos(3, 3), character: '^' },
            Error::UnexpectedChar { position: pos(4, 2), character: '😃' },
        ];
        compare_each(&errors, &expected);
    }

    #[test]
    fn no_unexpected_char() {
        let mut scanner = Scanner::default();
        let test_cases =
            [r#""no error inside string ^[]😃""#, r#"// no error inside comment ^[]😃"#];
        for test in test_cases {
            scanner.reset();
            assert!(scanner.scan(test).is_ok(), "there must be no unexpected character: {}", test);
        }
    }

    #[test]
    fn position_with_emoji() {
        let tokens = Scanner::default().scan(r#""🚁 😃"+or"#).unwrap();
        let expected = [
            Token::new(TokenType::String, "🚁 😃", pos(1, 1)),
            Token::new(TokenType::Plus, '+', pos(1, 6)),
            Token::new(TokenType::OR, "or", pos(1, 7)),
        ];
        compare_each(&tokens, &expected);
    }

    #[test]
    fn variables() {
        let tokens = Scanner::default()
            .scan(
                r#"
var var_with_underscore;
var var_with_numbers123;
                "#,
            )
            .unwrap();
        let expected = [
            Token::new(TokenType::Var, "var", pos(2, 1)),
            Token::new(TokenType::Identifier, "var_with_underscore", pos(2, 5)),
            Token::new(TokenType::Semicolon, ";", pos(2, 24)),
            Token::new(TokenType::Var, "var", pos(3, 1)),
            Token::new(TokenType::Identifier, "var_with_numbers123", pos(3, 5)),
            Token::new(TokenType::Semicolon, ";", pos(3, 24)),
        ];
        compare_each(&tokens, &expected);
    }
}
