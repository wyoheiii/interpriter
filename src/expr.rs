use crate::token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
  Literal(Literal),
  Grouping(Grouping),
  Unary(Unary),
  Binary(Binary),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Grouping {
  pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
  pub left: Box<Expr>,
  pub operator: BinaryOperator,
  pub right: Box<Expr>,
  pub left_token: token::Token,
  pub right_token: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
  pub operator: UnaryOperator,
  pub right: Box<Expr>,
  pub token: token::Token,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum UnaryOperator {
  Bang,
  Minus,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum BinaryOperator {
  Plus,
  Minus,
  Star,
  Slash,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  EqualEqual,
  BangEqual,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
  String(String),
  Number(f64),
  True,
  False,
  Nil,
}