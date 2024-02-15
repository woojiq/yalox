pub mod args;
pub mod ast;
pub mod interpreter;
pub mod parser;
pub mod resolver;
pub mod runner;
pub mod scanner;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub row: usize,
    pub col: usize,
}

impl Position {
    pub fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

impl From<(usize, usize)> for Position {
    fn from((row, col): (usize, usize)) -> Self {
        Position::new(row, col)
    }
}

/// Trait that complements [`Iterator`] and supports peeking.
pub trait MultiPeekable: Iterator
where
    Self::Item: PartialEq,
{
    fn peek_nth(&self, idx: usize) -> Option<&Self::Item>;

    fn peek(&self) -> Option<&Self::Item> {
        self.peek_nth(0)
    }

    fn next_if<F>(&mut self, func: F) -> Option<Self::Item>
    where
        F: FnOnce(&Self::Item) -> bool,
    {
        if self.peek().is_some_and(func) {
            self.next()
        } else {
            None
        }
    }

    fn next_if_eq(&mut self, item: &Self::Item) -> Option<Self::Item> {
        self.next_if(|v| v == item)
    }

    fn next_if_ne(&mut self, item: &Self::Item) -> Option<Self::Item> {
        self.next_if(|v| v != item)
    }
}

#[cfg(test)]
pub fn compare_each<T: PartialEq + std::fmt::Debug>(actual: &[T], expected: &[T]) {
    assert_eq!(actual.len(), expected.len());
    for (actual, expected) in actual.iter().zip(expected) {
        assert_eq!(actual, expected);
    }
}
