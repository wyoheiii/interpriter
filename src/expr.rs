use crate::token::{Token};

/*
expression -> literal | unary | binary | grouping ;
literal    -> NUMBER | STRING | "true" | "false" | "nil" ;
grouping   -> "(" expression ")" ;
unary      -> ("-" | "!") expression ;
binary     -> expression operator expression ;
operator   -> "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "/" ;
*/




#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
  Literal(Literal),
  Grouping(Grouping),
  Unary(Unary),
  Binary(Binary),
}

#[derive(Debug, Clone, PartialEq)]
pub struct  Grouping {
  pub expression: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
  pub left: Box<Expr>,
  pub operator: BinaryOperator,
  pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
  operator: UnaryOperator,
  right: Box<Expr>,
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

pub trait Visitor<T> {
  fn visit_expr(&self, expr: &Expr) -> T;
  fn visit_literal(&self, literal: &Literal) -> T;
  fn visit_grouping(&self, grouping: &Grouping) -> T;
  fn visit_unary(&self, unary: &Unary) -> T;
  fn visit_binary(&self, binary: &Binary) -> T;
}

impl Expr {
  pub fn accept<T>(&self, visitor: &dyn Visitor<T>) -> T {
    match self {
      Expr::Literal(literal) => visitor.visit_literal(literal),
      Expr::Grouping(grouping) => visitor.visit_grouping(grouping),
      Expr::Unary(unary) => visitor.visit_unary(unary),
      Expr::Binary(binary) => visitor.visit_binary(binary),
    }
  }
}

pub struct PrettyPrinter;
impl Visitor<String> for PrettyPrinter {
  fn visit_expr(&self, expr: &Expr) -> String {
    match expr {
      Expr::Literal(literal) => self.visit_literal(literal),
      Expr::Grouping(grouping) => self.visit_grouping(grouping),
      Expr::Unary(unary) => self.visit_unary(unary),
      Expr::Binary(binary) => self.visit_binary(binary),
    }
  }

  fn visit_literal(&self, literal: &Literal) -> String {
    match literal {
      Literal::String(s) => format!("\"{}\"", s),
      Literal::Number(n) => n.to_string(),
      Literal::True => "true".to_string(),
      Literal::False => "false".to_string(),
      Literal::Nil => "nil".to_string(),
    }
  }

  fn visit_grouping(&self, grouping: &Grouping) -> String {
    format!("({})", grouping.expression.accept(self))
  }

  fn visit_unary(&self, unary: &Unary) -> String {
    format!("{:?} {}", unary.operator, unary.right.accept(self))
  }

  fn visit_binary(&self, binary: &Binary) -> String {
    format!("{} {:?} {}", binary.left.accept(self), binary.operator, binary.right.accept(self))
  }
}
