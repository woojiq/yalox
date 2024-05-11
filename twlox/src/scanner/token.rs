use super::Position;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    type_: TokenType,
    lexeme: String,
    start_position: Position,
}

impl Token {
    pub fn new<S: ToString>(type_: TokenType, lexeme: S, start_position: Position) -> Self {
        Self { type_, lexeme: lexeme.to_string(), start_position }
    }

    pub fn get_type(&self) -> TokenType {
        self.type_
    }

    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }

    pub fn pos(&self) -> Position {
        self.start_position
    }
}

pub fn get_keyword(name: &str) -> Option<TokenType> {
    match name {
        "and" => Some(TokenType::And),
        "class" => Some(TokenType::Class),
        "else" => Some(TokenType::Else),
        "false" => Some(TokenType::False),
        "for" => Some(TokenType::For),
        "fun" => Some(TokenType::Fun),
        "if" => Some(TokenType::IF),
        "nil" => Some(TokenType::Nil),
        "or" => Some(TokenType::OR),
        "return" => Some(TokenType::Return),
        "super" => Some(TokenType::Super),
        "this" => Some(TokenType::This),
        "true" => Some(TokenType::True),
        "var" => Some(TokenType::Var),
        "while" => Some(TokenType::While),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    IF,
    Nil,
    OR,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}
