use std::{fmt, hash::Hash};
use crate::token::token_type::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
  String(String),
  Number(f64),
  Identifier(String),
  Nil,
}

impl fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Literal::String(s) => write!(f, "{}", s),
      Literal::Number(n) => write!(f, "{}", n),
      Literal::Identifier(id) => write!(f, "{}", id),
      Literal::Nil => write!(f, "Nil"),
    }
  }
}

#[derive(Debug, Clone)]
pub struct Token {
  pub token_type: TokenType,
  pub lexeme: String,
  pub line: usize,
  pub column: usize,
  pub literal: Option<Literal>,
}

impl PartialEq for Token {
  fn eq(&self, other: &Self) -> bool {
    self.token_type == other.token_type &&
    self.lexeme == other.lexeme &&
    self.line == other.line &&
    self.column == other.column
  }
}

impl Eq for Token {}

impl Hash for Token {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    (self.token_type, self.lexeme.as_bytes(), self.line, self.column).hash(state)
  }
}


impl Token {
  pub fn new(token_type: TokenType, lexeme: String, line: usize, column: usize, literal: Option<Literal>) -> Self {
    Token {
      token_type,
      lexeme,
      line,
      column,
      literal,
    }
  }

  pub fn this_dummy() -> Self {
    Token {
      token_type: TokenType::This,
      lexeme: "this".to_string(),
      line: 0,
      column: 0,
      literal: None,
    }
  }

  pub fn super_dummy() -> Self {
    Token {
      token_type: TokenType::Super,
      lexeme: "super".to_string(),
      line: 0,
      column: 0,
      literal: None,
    }
  }

  pub fn to_string(&self) -> String {
    format!("{:?} {} {}", self.token_type, self.lexeme, self.literal.as_ref().map_or("None".to_string(), |l| l.to_string()))
  }
}
