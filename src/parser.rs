use crate::token::{self, Token, TokenType};
use crate::expr::{
  Assign, Binary, BinaryOperator, Block, Call, Expr, FunDecl, Grouping, If, Literal, Logical, LogicalOperator, Stmt, Unary, UnaryOperator, VarDecl, Variable, While, Return, ClassDecl, Get, Set, This, Super
};
use std::fmt;



#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
  message: String,
  token: Token,
}

impl ParseError {
  fn new(message: &str, token: Token) -> Self {
    ParseError {
      message: message.to_string(),
      token,
    }
  }
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Parse error at {}:{}: {}: {}", self.token.line, self.token.column, self.token.lexeme, self.message)
  }
}

#[derive(Debug)]
pub struct Parser {
  tokens: Vec<Token>,
  current: usize,
  errs: Vec<ParseError>,
}

/*
program     -> declaration* EOF ;
declaration -> classDecl | funDecl | var_decl | statement ;
classDecl  -> "class" IDENTIFIER ("<" IDENTIFIER)? "{" function* "}" ;
funDecl     -> "fun" function ;
function    -> IDENTIFIER "(" parameters? ")" block ;
parameters  -> IDENTIFIER ("," IDENTIFIER)* ;
var_decl    -> "var" IDENTIFIER "=" expression ";" ;
statement   -> expr_stmt | print_stmt | block | if_stmt | while_stmt | for_stmt | return_stmt ;
return_stmt -> "return" expression? ";" ;
for_stmt    -> "for" "(" (var_decl | expr_stmt | ";") expression? ";" expression? ")" statement ;
while_stmt  -> "while" "(" expression ")" statement ;
if_stmt     -> "if" "(" expression ")" statement ("else" statement)? ;
block       -> "{" declaration* "}" ;
expr_stmt   -> expression ";" ;
print_stmt  -> "print" expression ";" ;
expression  -> assignment ;
assignment  -> (call ".")? IDENTIFIER "=" assignment | logic_or ;
logic_or    -> logic_and ("or" logic_and)* ;
logic_and   -> equality ("and" equality)* ;
equality    -> comparison (("==" | "!=") comparison)* ;
comparison  -> term (("<" | "<=" | ">" | ">=") term)* ;
term        -> factor (("-" | "+") factor)* ;
factor      -> unary (("*" | "/") unary)* ;
unary       -> ("-" | "!") unary | call ;
call        -> primary ("(" arguments? ")" | "."IDENTIFIER)* ;
arguments   -> expression ("," expression)* ;
primary     -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER | "super" "." IDENTIFIER;
*/

type ExprResult = Result<Expr, ParseError>;
type StmtResult = Result<Stmt, ParseError>;

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Parser { tokens, current: 0, errs: Vec::new() }
  }

  pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<ParseError>> {
    let mut stmts = Vec::new();
    while !self.is_at_end() {
      if let Some(stmt) = self.declaration().ok() {
        stmts.push(stmt);
      }
    }

    if !self.errs.is_empty() {
      return Err(self.errs.clone());
    }
    Ok(stmts)
  }

  fn declaration(&mut self) -> StmtResult {
    let res = if self.match_token(&[TokenType::Var]) {
      self.var_decl()
    }  else if self.match_token(&[TokenType::Fun]) {
      self.fun_decl("function")
    } else if self.match_token(&[TokenType::Return]) {
      self.ret_stmt()
    } else if self.match_token(&[TokenType::Class]) {
      self.class_decl()
    } else {
      self.stmt()
    };

    if res.is_err() {
      self.errs.push(res.clone().err().unwrap());
      self.synchronize();
    }

    res
  }

  fn class_decl(&mut self) -> StmtResult {
    let name = self.consume(TokenType::Identifier, "Expect class name.")?.clone();

    let super_class = if self.match_token(&[TokenType::Less]) {
      self.consume(TokenType::Identifier, "Expect superclass name.")?;
      Some(Expr::Variable(
        Variable {
          name: self.previous().clone(),
        }
      ))
    } else {
      None
    };

    self.consume(TokenType::LeftBrace, "Expect '{' before class body.")?;

    let mut methods = Vec::new();
    while !self.is_at_end() && !self.check(TokenType::RightBrace) {
      match self.fun_decl("method") {
        Ok(Stmt::FunDecl(method)) => methods.push(method),
        Err(e) => {
          return Err(e);
        }
        _ => {
          return Err(ParseError::new("Expect method declaration.", self.peek().clone()));
        }
      }
    }

    self.consume(TokenType::RightBrace, "Expect '}' after class body.")?;

    Ok(Stmt::ClassDecl(
      ClassDecl {
        name,
        methods,
        super_class,
      }
    ))
  }

  fn ret_stmt(&mut self) -> StmtResult {
    let keyword = self.previous().clone();
    let value = if !self.check(TokenType::Semicolon) {
      Some(Box::new(self.expression()?))
    } else {
      None
    };

    self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
    Ok(Stmt::Return(
      Return {
        keyword,
        value,
      }
    ))
  }

  fn fun_decl(&mut self, kind: &str) -> StmtResult {
    let name = self.consume(TokenType::Identifier, &format!("Expect {} name.", kind))?.clone();
    self.consume(TokenType::LeftParen, &format!("Expect '(' after {} name.", kind))?;
    let mut params = Vec::new();

    if !self.check(TokenType::RightParen) {
      loop {
        if params.len() >= 255 {
          return Err(ParseError::new("Can't have more than 255 parameters.", self.peek().clone()));
        }

        params.push(self.consume(TokenType::Identifier, "Expect parameter name.")?.clone());
        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
    self.consume(TokenType::LeftBrace, "Expect '{' before function body.")?;

    let body = self.block()?;

    Ok(Stmt::FunDecl(
      FunDecl {
        name,
        params,
        body: Block {
          stmts: match body {
            Stmt::Block(b) => b.stmts,
            _ => vec![body],
          },
        },
      }
    ))
  }

  fn var_decl(&mut self) -> StmtResult {
    let name = self.consume(TokenType::Identifier, "Expect variable name.")?.clone();
    let initializer = if self.match_token(&[TokenType::Equal]) {
      Some(Box::new(self.expression()?))
    } else {
      None
    };

    self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.")?;
    Ok(Stmt::VarDecl(
      VarDecl {
        name: name,
        initializer,
      }
    ))
  }

  fn stmt(&mut self) ->StmtResult {
    if self.match_token(&[TokenType::Print]) {
      return self.print_stmt();
    } else if self.match_token(&[TokenType::LeftBrace]) {
      return self.block();
    } else if self.match_token(&[TokenType::If]) {
      return self.if_stmt();
    } else if self.match_token(&[TokenType::While]) {
      return self.while_stmt();
    } else if self.match_token(&[TokenType::For]) {
      return self.for_stmt();
    }
    self.expr_stmt()
  }

  fn for_stmt(&mut self) -> StmtResult {
    self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

    let initializer = if self.match_token(&[TokenType::Var]) {
      Some(Box::new(self.var_decl()?))
    } else if self.match_token(&[TokenType::Semicolon]) {
      None
    } else {
      Some(Box::new(self.expr_stmt()?))
    };

    let condition = if self.check(TokenType::Semicolon) {
      None
    } else {
      Some(Box::new(self.expression()?))
    };

    self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;
    let increment = if self.check(TokenType::RightParen) {
      None
    } else {
      Some(Box::new(self.expression()?))
    };

    self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;


    let mut body = self.stmt()?;
    if let Some(increment) = increment {
      body = Stmt::Block(
        Block {
          stmts: vec![body, Stmt::Expr(*increment)],
        }
      );
    };

    if let Some(condition) = condition {
      body = Stmt::While(
        While {
          condition: *condition,
          body: Box::new(body),
        }
      );
    };

    if let Some(initializer) = initializer {
      body = Stmt::Block(
        Block {
          stmts: vec![*initializer, body],
        }
      );
    }

    Ok(body)
  }

  fn while_stmt(&mut self) -> StmtResult {
    self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
    let condition = self.expression()?;
    self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

    Ok(Stmt::While(
      While {
        condition,
        body: Box::new(self.stmt()?),
      }
    ))

  }

  fn if_stmt(&mut self) -> StmtResult {
    self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
    let condition = self.expression()?;
    self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

    let  then = self.stmt()?;
    let else_branch = if self.match_token(&[TokenType::Else]) {
      Some(Box::new(self.stmt()?))
    } else {
      None
    };

    Ok(Stmt::If(
      If {
        condition,
        then_branch: Box::new(then),
        else_branch,
      }
    ))
  }

  fn block(&mut self) -> StmtResult {
    let mut stmts = Vec::new();
    while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
      if let Some(stmt) = self.declaration().ok() {
        stmts.push(stmt);
      }
    }

    self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
    Ok(Stmt::Block(
      Block {
        stmts,
      }
    ))
  }

  fn print_stmt(&mut self) ->StmtResult {
    let value = self.expression()?;
    self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
    Ok(Stmt::Print(value))
  }

  fn expr_stmt(&mut self) ->StmtResult {
    let expr = self.expression()?;
    self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
    Ok(Stmt::Expr(expr))
  }

  fn expression(&mut self) -> ExprResult {
    Ok(self.assignment()?)
  }

  fn assignment(&mut self) -> ExprResult {
    let expr = self.or()?;
    if self.match_token(&[TokenType::Equal]) {
      let equals = self.previous().clone();
      let value = self.assignment()?;
      if let Expr::Variable(var) = expr {
        return Ok(Expr::Assign(
          Assign {
            name: var.name,
            value: Box::new(value),
          }
        ));
      } else if let Expr::Get(g) = expr {
        return Ok(Expr::Set(
          Set {
            object: g.object,
            name: g.name,
            value: Box::new(value),
          }
        ));
      }
      Err(ParseError::new("Invalid assignment target.", equals))
    } else {
      Ok(expr)
    }
  }

  fn or(&mut self) -> ExprResult {
    let mut expr = self.and()?;

    while self.match_token(&[TokenType::Or]) {
      let right = self.and()?;
      expr = Expr::Logical (
        Logical {
          left: Box::new(expr),
          operator: LogicalOperator::Or,
          right: Box::new(right),
          left_token: self.before_before_previous().clone(),
          right_token: self.previous().clone()
        }
      );
    }

    Ok(expr)
  }

  fn and(&mut self) ->ExprResult {
    let mut expr = self.equality()?;

    while self.match_token(&[TokenType::And]) {
      let right = self.equality()?;
      expr = Expr::Logical (
        Logical {
          left: Box::new(expr),
          operator: LogicalOperator::And,
          right: Box::new(right),
          left_token: self.before_before_previous().clone(),
          right_token: self.previous().clone()
        }
      );
    }

    Ok(expr)
  }

  fn equality(&mut self) -> ExprResult {
    self.parse_binary( |p| p.comparison(), &[
      TokenType::EqualEqual,
      TokenType::BangEqual,
    ])
  }

  fn comparison(&mut self) -> ExprResult {
    self.parse_binary( |p| p.term(), &[
      TokenType::Less,
      TokenType::LessEqual,
      TokenType::Greater,
      TokenType::GreaterEqual,
    ])
  }

  fn term(&mut self) -> ExprResult {
    self.parse_binary( |p| p.factor(), &[
      TokenType::Plus,
      TokenType::Minus,
    ])
  }

  fn factor(&mut self) -> ExprResult {
    self.parse_binary( |p| p.unary(), &[
      TokenType::Star,
      TokenType::Slash,
    ])
  }

  fn parse_binary<F>(&mut self, mut sub_expr: F, types: &[TokenType]) -> ExprResult
  where
    F: FnMut(&mut Self) -> ExprResult,
  {
    let mut expr = sub_expr(self)?;

    while self.match_token(types) {
      let operator = self.previous().clone();
      let right = sub_expr(self)?;
      expr = Expr::Binary(
        Binary {
          left: Box::new(expr),
          operator: self.convert_to_binary_op(&operator.token_type),
          right: Box::new(right),
          left_token: self.before_before_previous().clone(),
          right_token: self.previous().clone()
        }
      );
    }

    Ok(expr)
  }

  fn unary(&mut self)-> ExprResult {
    if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
      let operator = self.previous().clone();
      let right = self.unary()?;
      return Ok(Expr::Unary(
        Unary {
          operator: match operator.token_type {
            TokenType::Bang => UnaryOperator::Bang,
            TokenType::Minus => UnaryOperator::Minus,
            _ => panic!("Invalid operator"),
          },
          right: Box::new(right),
          token: self.previous().clone(),
        }
      ));
    }

    self.call()
  }

  fn call(&mut self) -> ExprResult {
    let mut expr = self.primary()?;

    loop {
      if self.match_token(&[TokenType::LeftParen]) {
        expr = self.finish_call(expr)?;
      } else if self.match_token(&[TokenType::Dot]){
        let name = self.consume(TokenType::Identifier, "Expect property name after '.'")?.clone();

        expr = Expr::Get(
          Get {
            object: Box::new(expr),
            name,
          }
        );
      }else {
        break;
      }
    }

    Ok(expr)
  }

  fn finish_call(&mut self, callee: Expr) -> ExprResult {
    let mut args = Vec::new();
    if !self.check(TokenType::RightParen) {
      loop {
        args.push(self.expression()?);

        if args.len() > 255 {
          return Err(ParseError::new("Can't have more than 255 arguments.", self.peek().clone()));
        }

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.")?.clone();
    Ok(Expr::Call(Call{
      callee: Box::new(callee),
      paren,
      args,
    }))
  }

  fn primary(&mut self) -> ExprResult {
    if self.match_token(&[TokenType::False]) {
      return Ok(Expr::Literal(Literal::False));
    }
    if self.match_token(&[TokenType::True]) {
      return Ok(Expr::Literal(Literal::True));
    }
    if self.match_token(&[TokenType::Nil]) {
      return Ok(Expr::Literal(Literal::Nil));
    }
    if self.match_token(&[TokenType::Number]) {
      match self.previous().literal {
        Some(token::Literal::Number(number)) => return Ok(Expr::Literal(Literal::Number(number))),
        Some(_) => panic!("internal error: number literal is not a number"),
        None => panic!("internal error: number literal is None"),
      }
    }
    if self.match_token(&[TokenType::String]) {
      match &self.previous().literal {
        Some(token::Literal::String(s)) => return Ok(Expr::Literal(Literal::String(s.clone()))),
        Some(_) => panic!("internal error: string literal is not a string"),
        None => panic!("internal error: string literal is None"),
      }
    }

    if self.match_token(&[TokenType::LeftParen]) {
      let expr = self.expression()?;
      self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
      return Ok(Expr::Grouping(
        Grouping {
          expr: Box::new(expr),
        }
        ));
    }

    if self.match_token(&[TokenType::This]) {
      return Ok(Expr::This(
        This {
          keyword: self.previous().clone(),
        }
      ));
    }

    if self.match_token(&[TokenType::Identifier]) {
      return Ok(Expr::Variable(
        Variable {
          name: self.previous().clone(),
        }
        ));
    }

    if self.match_token(&[TokenType::Super]) {
      let keyword = self.previous().clone();
      self.consume(TokenType::Dot, "Expect '.' after 'super'.")?;
      let method = self.consume(TokenType::Identifier, "Expect superclass method name.")?.clone();

      return Ok(Expr::Super(
        Super {
          keyword,
          method,
        }
      ));
    }

    // to
    Err(ParseError::new("Expect expression.", self.peek().clone()))
  }

  fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, ParseError> {
    if self.check(token_type) {
      return Ok(self.advance());
    }
    Err(ParseError { message: message.to_string(), token: self.peek().clone() })
  }

  fn synchronize(&mut self) {
    self.advance();
    while !self.is_at_end() {
      if self.previous().token_type == TokenType::Semicolon {
        return;
      }

      match self.peek().token_type {
        TokenType::Class |
        TokenType::For |
        TokenType::Fun |
        TokenType::If |
        TokenType::While |
        TokenType::Print |
        TokenType::Return => return,
        _ => {}
      }
      self.advance();
    }
  }

  fn match_token(&mut self, types: &[TokenType]) -> bool {
    for t in types {
      if self.check(*t) {
        self.advance();
        return true;
      }
    }
    false
  }

  fn check(&self, token_type: TokenType) -> bool {
    if self.is_at_end() {
      return false;
    }
    self.peek().token_type == token_type
  }

  fn peek(&self) -> &Token {
    &self.tokens[self.current]
  }

  fn advance(&mut self)-> &Token {
    self.current += 1;
    self.previous()
  }

  fn previous(&mut self) -> &Token {
    &self.tokens[self.current - 1]
  }

  fn before_before_previous(&mut self) -> &Token {
    &self.tokens[self.current - 3]
  }

  fn is_at_end(&self) -> bool {
    self.peek().token_type == TokenType::Eof
  }

  fn convert_to_binary_op(&self, op: &TokenType) -> BinaryOperator {
    match op {
      TokenType::Plus => BinaryOperator::Plus,
      TokenType::Minus => BinaryOperator::Minus,
      TokenType::Star => BinaryOperator::Star,
      TokenType::Slash => BinaryOperator::Slash,
      TokenType::EqualEqual => BinaryOperator::EqualEqual,
      TokenType::BangEqual => BinaryOperator::BangEqual,
      TokenType::Less => BinaryOperator::Less,
      TokenType::LessEqual => BinaryOperator::LessEqual,
      TokenType::Greater => BinaryOperator::Greater,
      TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
      _ => panic!("Invalid operator"),
    }
  }
}