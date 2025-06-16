use crate::token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
  Literal(Literal),
  Grouping(Grouping),
  Unary(Unary),
  Binary(Binary),
  Variable(Variable),
  Assign(Assign),
  Logical(Logical),
  Call(Call),
  Get(Get),
  Set(Set),
  This(This),
  Super(Super),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
  Expr(Expr),
  Print(Expr),
  VarDecl(VarDecl),
  FunDecl(FunDecl),
  Block(Block),
  If(If),
  While(While),
  Return(Return),
  ClassDecl(ClassDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
  pub name: token::Token,
  pub super_class: Option<Expr>, // variable
  pub methods: Vec<FunDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
  pub keyword: token::Token,
  pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunDecl {
  pub name: token::Token,
  pub params: Vec<token::Token>,
  pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
  pub condition: Expr,
  pub body: Box<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
  pub condition: Expr,
  pub then_branch: Box<Stmt>,
  pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
  pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
  pub name: token::Token,
  pub initializer: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Super {
  pub keyword: token::Token,
  pub method: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct This {
  pub keyword: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Set {
  pub object: Box<Expr>,
  pub name: token::Token,
  pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Get {
  pub object: Box<Expr>,
  pub name: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
  pub callee: Box<Expr>,
  pub paren: token::Token,
  pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Logical {
  pub left: Box<Expr>,
  pub operator: LogicalOperator,
  pub right: Box<Expr>,
  pub left_token: token::Token,
  pub right_token: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
  pub name: token::Token,
  pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
  pub name: token::Token,
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

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOperator {
  And,
  Or,
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