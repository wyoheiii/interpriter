use crate::token;
use std::fmt;

pub struct Scanner {
  source: Vec<u8>,
  tokens: Vec<token::Token>,
  err: Vec<ScannerError>,
  start: usize,
  current: usize,
  column: usize,
  line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScannerError {
  message: String,
  line: usize,
  column: usize,
}

impl fmt::Display for ScannerError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "scan error at {}:{}: {}", self.line, self.column, self.message)
  }
}

impl Scanner {
  pub fn new(source: &str) -> Self {
    Scanner {
      source: source.as_bytes().to_vec(),
      tokens: Vec::new(),
      err: Vec::new(),
      start: 0,
      current: 0,
      column: 0,
      line: 1,
    }
  }

  pub fn scan_tokens(&mut self) -> Result<Vec<token::Token>, Vec<ScannerError>> {

    while !self.is_at_end() {
      self.start = self.current;
      self.scan_token();
    }

    self.tokens.push(token::Token::new(
        token::TokenType::Eof,
        String::from(""),
        self.line,
        self.column,
        None,
      ));

    if !self.err.is_empty() {
      return Err(self.err.clone());
    }

    Ok(self.tokens.clone())
  }

  fn scan_token(&mut self) {
    let c = self.advance();

    match c {
      b'(' => self.add_token(token::TokenType::LeftParen),
      b')' => self.add_token(token::TokenType::RightParen),
      b'{' => self.add_token(token::TokenType::LeftBrace),
      b'}' => self.add_token(token::TokenType::RightBrace),
      b',' => self.add_token(token::TokenType::Comma),
      b'.' => self.add_token(token::TokenType::Dot),
      b'-' => self.add_token(token::TokenType::Minus),
      b'+' => self.add_token(token::TokenType::Plus),
      b';' => self.add_token(token::TokenType::Semicolon),
      b'*' => self.add_token(token::TokenType::Star),
      b'!' => {
        let ty = if self.matches(b'=') {
          token::TokenType::BangEqual
        } else {
          token::TokenType::Bang
        };

        self.add_token(ty);
      }
      b'=' => {
        let ty = if self.matches(b'=') {
          token::TokenType::EqualEqual
        } else {
          token::TokenType::Equal
        };

        self.add_token(ty);
      }
      b'<' => {
        let ty = if self.matches(b'=') {
          token::TokenType::LessEqual
        } else {
          token::TokenType::Less
        };

        self.add_token(ty);
      }
      b'>' => {
        let ty = if self.matches(b'=') {
          token::TokenType::GreaterEqual
        } else {
          token::TokenType::Greater
        };

        self.add_token(ty);
      }
      b'/' => {
        if self.matches(b'/') {
          // A comment goes until the end of the line.
          while self.peek() != b'\n' && !self.is_at_end() {
            self.advance();
          }
        } else if self.matches(b'*') {
          let mut depth = 1;
          while !self.is_at_end() {

            if self.peek() == b'*' && self.peek_next() == b'/' {
              // Consume the closing */.
              self.advance();
              self.advance();
              depth -= 1;
              if depth == 0 {
                break;
              }
            } else if self.peek() == b'/' && self.peek_next() == b'*' {
              // Consume the opening /*.
              self.advance();
              self.advance();
              depth += 1;
            } else if self.peek() == b'\n' {
              self.advance();
              self.line += 1;
              self.column = 0;
            } else {
              self.advance();
            }
          }
          if depth > 0 {
            self.err.push(ScannerError {
              message: String::from("Unterminated block comment."),
              line: self.line,
              column: self.column,
            });
          }
        }
        else {
          self.add_token(token::TokenType::Slash);
        }
      }
      b' ' | b'\r' | b'\t' => {
        // Ignore whitespace.
      }
      b'\n' => {
        self.line += 1;
        self.column = 0;
      }
      b'"' => {
        self.string();
      }

      _=> {
        if self.is_digit(c) {
          self.number();
        } else if self.is_alpha(c) {
          self.identifier();
        } else {
          self.err.push(ScannerError {
            message: String::from("Unexpected character."),
            line: self.line,
            column: self.column,
          });
        }
      }
    }
  }


  fn add_token(&mut self, ty: token::TokenType) {
    self.add_token_with_literal(ty, None);
  }

  fn add_token_with_literal(&mut self, ty: token::TokenType, literal: Option<token::Literal>) {
    let text = String::from_utf8_lossy(&self.source[self.start..self.current]).to_string();
    self.tokens.push(token::Token::new(
      ty,
      text,
      self.line,
      self.start,
      literal,
    ));
  }

  fn identifier(&mut self) {
    while self.is_alpha_numeric(self.peek()) {
      self.advance();
    }

    let text = String::from_utf8_lossy(&self.source[self.start..self.current]).to_string();
    let keyword = self.get_keyword(&text);
    self.add_token(keyword);
  }

  fn string(&mut self) {
    while self.peek() != b'"' && !self.is_at_end() {
      if self.peek() == b'\n' {
        self.line += 1;
        self.column = 0;
      }
      self.advance();
    }

    // The closing ".
    if self.is_at_end() {
      self.err.push(ScannerError {
        message: String::from("Unterminated string."),
        line: self.line,
        column: self.column,
      });
      return;
    }

    // Consume the closing ".
    self.advance();

    let value = String::from_utf8_lossy(&self.source[self.start + 1..self.current - 1]).to_string();
    self.add_token_with_literal(token::TokenType::String, Some(token::Literal::String(value)));
  }


  fn number(&mut self) {
    while self.is_digit(self.peek()) {
      self.advance();
    }

    if self.peek() == b'.' && self.is_digit(self.peek_next()) {
      // Consume the '.'.
      self.advance();
      while self.is_digit(self.peek()) {
        self.advance();
      }
    }

    let value = String::from_utf8_lossy(&self.source[self.start..self.current]).to_string();
    let number = value.parse::<f64>();
    match number {
      Ok(n) => {
        self.add_token_with_literal(token::TokenType::Number, Some(token::Literal::Number(n)));
      }
      Err(_) => {
        self.err.push(ScannerError {
          message: String::from("Invalid number."),
          line: self.line,
          column: self.column,
        });
      }
    }
  }

  fn get_keyword(&self, keyword: &str) -> token::TokenType {
    match keyword {
      "and" => token::TokenType::And,
      "class" => token::TokenType::Class,
      "else" => token::TokenType::Else,
      "false" => token::TokenType::False,
      "fun" => token::TokenType::Fun,
      "for" => token::TokenType::For,
      "if" => token::TokenType::If,
      "nil" => token::TokenType::Nil,
      "or" => token::TokenType::Or,
      "print" => token::TokenType::Print,
      "return" => token::TokenType::Return,
      "super" => token::TokenType::Super,
      "this" => token::TokenType::This,
      "true" => token::TokenType::True,
      "var" => token::TokenType::Var,
      "while" => token::TokenType::While,
      _ => token::TokenType::Identifier,
    }
  }

  fn is_digit(&self, c: u8) -> bool {
    c.is_ascii_digit()
  }

  fn is_alpha(&self, c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
  }

  fn is_alpha_numeric(&self, c: u8) -> bool {
    self.is_alpha(c) || self.is_digit(c) || c == b'_'
  }

  fn advance(&mut self) -> u8 {
    let c = self.source[self.current];
    self.current += 1;
    self.column += 1;
    c
  }

  fn peek(&self) -> u8 {
    if self.is_at_end() {
      return b'\0';
    }

    self.source[self.current]
  }

  fn peek_next(&self) -> u8 {
    if (self.current + 1) >= self.source.len() {
      return b'\0';
    }

    self.source[self.current + 1]
  }

  fn matches(&mut self, expected: u8) -> bool {
    if self.is_at_end() {
      return false;
    }

    if self.source[self.current] != expected {
      return false;
    }

    self.current += 1;
    self.column += 1;
    true
  }

  fn is_at_end(&self) -> bool {
    self.current >= self.source.len()
  }

}


#[cfg(test)]
mod tests {
  use super::*;
  use crate::token::TokenType;

  #[test]
  fn test_scan_tokens() {
    let source = r#"
      var x = 10;
      var y = "hello";
      if (x <= y) {
        print x;
      }
      /* This is a comment */
    "#;


    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().unwrap();

    assert_eq!(tokens.len(), 22);
    assert_eq!(tokens[0].token_type, TokenType::Var);
    assert_eq!(tokens[1].token_type, TokenType::Identifier);
    assert_eq!(tokens[2].token_type, TokenType::Equal);
    assert_eq!(tokens[3].token_type, TokenType::Number);
    assert_eq!(tokens[4].token_type, TokenType::Semicolon);
    assert_eq!(tokens[5].token_type, TokenType::Var);
    assert_eq!(tokens[6].token_type, TokenType::Identifier);
    assert_eq!(tokens[7].token_type, TokenType::Equal);
    assert_eq!(tokens[8].token_type, TokenType::String);
    assert_eq!(tokens[9].token_type, TokenType::Semicolon);
    assert_eq!(tokens[10].token_type, TokenType::If);
    assert_eq!(tokens[11].token_type, TokenType::LeftParen);
    assert_eq!(tokens[12].token_type, TokenType::Identifier);
    assert_eq!(tokens[13].token_type, TokenType::LessEqual);
    assert_eq!(tokens[14].token_type, TokenType::Identifier);
    assert_eq!(tokens[15].token_type, TokenType::RightParen);
    assert_eq!(tokens[16].token_type, TokenType::LeftBrace);
    assert_eq!(tokens[17].token_type, TokenType::Print);
    assert_eq!(tokens[18].token_type, TokenType::Identifier);
    assert_eq!(tokens[19].token_type, TokenType::Semicolon);
    assert_eq!(tokens[20].token_type, TokenType::RightBrace);
    assert_eq!(tokens[21].token_type, TokenType::Eof);
  }
}