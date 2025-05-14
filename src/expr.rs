
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct SourcePos {
  pub line: usize,
  pub column: usize,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
  Literal(Literal),
  Grouping(Grouping),
  Unary(Unary),
  Binary(Binary),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Grouping {
  pub expression: Box<Expr>,
  pub pos: SourcePos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
  pub left: Box<Expr>,
  pub operator: BinaryOperator,
  pub right: Box<Expr>,
  pub pos: SourcePos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
  pub operator: UnaryOperator,
  pub right: Box<Expr>,
  pub pos: SourcePos,
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