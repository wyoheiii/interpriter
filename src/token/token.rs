use std::fmt;
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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
  token_type: TokenType,
  lexeme: String,
  line: usize,
  column: usize,
  literal: Option<Literal>,
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

  pub fn to_string(&self) -> String {
    format!("{:?} {} {}", self.token_type, self.lexeme, self.literal.as_ref().map_or("None".to_string(), |l| l.to_string()))
  }
}
