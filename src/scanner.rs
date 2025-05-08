use crate::token;

pub struct Scanner<'a> {
  source: &'a str,
  tokens: Vec<token::Token>,

  start: usize,
  current: usize,
  line: usize,
}

impl <'a> Scanner<'a> {
  pub fn new(source: &'a str) -> Self {
    Scanner {
      source,
      tokens: Vec::new(),
      start: 0,
      current: 0,
      line: 1,

    }
  }

  pub fn scan_tokens(&mut self) -> Vec<token::Token> {

    while !self.is_at_end() {
      self.start = self.current;
      self.scan_token();
    }

    self.tokens.push(token::Token::new(
        token::TokenType::Eof,
        String::from(""),
        self.line,
        self.start,
        token::Literal::Nil,
      ));
    self.tokens.clone()
  }

  fn scan_token(&mut self) {}

  fn is_at_end(&self) -> bool {
    self.current >= self.source.len()
  }

}