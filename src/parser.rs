use crate::token::{self, Token, TokenType};
use crate::expr::{Binary, BinaryOperator, Expr, Unary, UnaryOperator, Literal, Grouping, SourcePos};
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
expression -> equality ;
equality   -> comparison (("==" | "!=") comparison)* ;
comparison -> term (("<" | "<=" | ">" | ">=") term)* ;
term       -> factor (("-" | "+") factor)* ;
factor     -> unary (("*" | "/") unary)* ;
unary      -> ("-" | "!") unary | primary ;
primary    -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
*/

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Parser { tokens, current: 0, errs: Vec::new() }
  }

  pub fn parse(&mut self) -> Result<Expr, ParseError> {
    Ok(self.expression()?)
  }

  fn expression(&mut self) -> Result<Expr, ParseError> {
    Ok(self.equality()?)
  }

  fn equality(&mut self) -> Result<Expr, ParseError> {
    self.parse_binary( |p| p.comparison(), &[
      TokenType::EqualEqual,
      TokenType::BangEqual,
    ])
  }

  fn comparison(&mut self) -> Result<Expr, ParseError> {
    self.parse_binary( |p| p.term(), &[
      TokenType::Less,
      TokenType::LessEqual,
      TokenType::Greater,
      TokenType::GreaterEqual,
    ])
  }

  fn term(&mut self) -> Result<Expr, ParseError> {
    self.parse_binary( |p| p.factor(), &[
      TokenType::Plus,
      TokenType::Minus,
    ])
  }

  fn factor(&mut self) -> Result<Expr, ParseError> {
    self.parse_binary( |p| p.unary(), &[
      TokenType::Star,
      TokenType::Slash,
    ])
  }

  fn parse_binary<F>(&mut self, mut sub_expr: F, types: &[TokenType]) -> Result<Expr, ParseError>
  where
    F: FnMut(&mut Self) -> Result<Expr, ParseError>,
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
          pos: SourcePos {
            line: operator.line,
            column: operator.column,
          },
        }
      );
    }

    Ok(expr)
  }

  fn unary(&mut self)-> Result<Expr, ParseError> {
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
          pos: SourcePos {
            line: operator.line,
            column: operator.column,
          },
        }
      ));
    }

    self.primary()
  }

  fn primary(&mut self) -> Result<Expr, ParseError> {
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
          expression: Box::new(expr),
          pos: SourcePos {
            line: self.previous().line,
            column: self.previous().column,
          },
        }
        ));
    }

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