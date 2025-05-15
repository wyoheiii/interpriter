use crate::expr::{Literal, Grouping, Binary, Unary, Expr, UnaryOperator, BinaryOperator};
use crate::token::Token;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum RunTimeError {
  UnaryError {
    token: Token,
    message: String,
  },
  BinaryError {
    left: Token,
    right: Token,
    operator: String,
    message: String,
  },
}

impl fmt::Display for RunTimeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      RunTimeError::UnaryError {token,  message } => {
        write!(f, "Run-time error at {}:{}: {}: {}", token.line, token.column, token.lexeme, message)
      }
      RunTimeError::BinaryError { left, right, operator ,message } => {
        write!(f, "Run-time error at {}:{}: {} {} {}: {}", left.line, left.column, left.lexeme, operator, right.lexeme, message)
      }
    }

  }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Value {
  Number(f64),
  String(String),
  Boolean(bool),
  Nil,
}

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", match self {
          Value::Number(n) => {
            let s = n.to_string();
            if s.ends_with(".0") {
              s[..s.len()-2].to_string()
            } else {
              s
            }
          },
          Value::String(s) => s.clone(),
          Value::Boolean(b) => b.to_string(),
          Value::Nil => "nil".to_string(),
      })
  }
}

#[derive(Debug)]
pub struct Interpreter {}

type ExprResult = Result<Value, RunTimeError>;
impl Interpreter {
  pub fn new() -> Self {
    Interpreter {}
  }

  fn interpret_expr(&self, expr: &Expr) -> ExprResult {
    match expr {
      Expr::Literal(literal) => self.interpret_literal(literal),
      Expr::Grouping(grouping) => self.interpret_expr(&grouping.expr),
      Expr::Unary(unary) => self.interpret_unary(unary),
      Expr::Binary(binary) => self.interpret_binary(binary),
    }
  }

  fn interpret_literal(&self, literal: &Literal) -> ExprResult {
    match literal {
      Literal::String(s) => Ok(Value::String(s.clone())),
      Literal::Number(n) => Ok(Value::Number(*n)),
      Literal::False => Ok(Value::Boolean(false)),
      Literal::True => Ok(Value::Boolean(true)),
      Literal::Nil => Ok(Value::Nil),
    }
  }

  fn interpret_unary(&self, unary: &Unary) -> ExprResult {
    let right = self.interpret_expr(&unary.right)?;
    match unary.operator {
      UnaryOperator::Minus => match right {
        Value::Number(n) => Ok(Value::Number(-n)),
        _ => Err(RunTimeError::UnaryError {
          token: unary.token.clone(),
          message: "operator '-' requires a number".to_string(),
        }),
      },
      UnaryOperator::Bang => Ok(Value::Boolean(!self.is_truthy(&right))),
    }
  }

  fn interpret_binary(&self, binary: &Binary) -> ExprResult {
    let left = self.interpret_expr(&binary.left)?;
    let right = self.interpret_expr(&binary.right)?;

    match binary.operator {
      BinaryOperator::Greater => match (left, right) {
        (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l > r)),
        _ => Err(RunTimeError::BinaryError {
          message: "operator '>' requires two numbers".to_string(),
          left: binary.left_token.clone(),
          right: binary.right_token.clone(),
          operator: ">".to_string(),
        }),
      },
      BinaryOperator::GreaterEqual => match (left, right) {
        (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l >= r)),
        _ => Err(RunTimeError::BinaryError {
          message: "operator '>=' requires two numbers".to_string(),
          left: binary.left_token.clone(),
          right: binary.right_token.clone(),
          operator: ">=".to_string(),
        }),
      },
      BinaryOperator::Less => match (left, right) {
        (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l < r)),
        _ => Err(RunTimeError::BinaryError {
          message: "operator '<' requires two numbers".to_string(),
          left: binary.left_token.clone(),
          right: binary.right_token.clone(),
          operator: "<".to_string(),
        }),
      },
      BinaryOperator::LessEqual => match (left, right) {
        (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l <= r)),
        _ => Err(RunTimeError::BinaryError {
          message: "operator '<=' requires two numbers".to_string(),
          left: binary.left_token.clone(),
          right: binary.right_token.clone(),
          operator: "<=".to_string(),
        }),
      },
      BinaryOperator::Minus => match (left, right) {
        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
        _ => Err(RunTimeError::BinaryError {
          message: "operand '-' requires two numbers".to_string(),
          left: binary.left_token.clone(),
          right: binary.right_token.clone(),
          operator: "-".to_string(),
        }),
      },
      BinaryOperator::Plus => match (left, right) {
        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
        (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
        _=> Err(RunTimeError::BinaryError {
          message: "operand '+' requires two numbers or two strings".to_string(),
          left: binary.left_token.clone(),
          right: binary.right_token.clone(),
          operator: "+".to_string(),
        }),
      }
      BinaryOperator::Star => match (left, right) {
        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
        _ => Err(RunTimeError::BinaryError {
          message: "operand '*' requires two numbers".to_string(),
          left: binary.left_token.clone(),
          right: binary.right_token.clone(),
          operator: "*".to_string(),
        }),
      }
      BinaryOperator::Slash => match (left, right) {
        (Value::Number(l), Value::Number(r)) => {
          if r == 0.0 {
            Err(RunTimeError::BinaryError {
              message: "Division by zero".to_string(),
              left: binary.left_token.clone(),
              right: binary.right_token.clone(),
              operator: "/".to_string(),
            })
          } else {
            Ok(Value::Number(l / r))
          }
        }
        _ => Err(RunTimeError::BinaryError {
          message: "operand '/' requires two numbers".to_string(),
          left: binary.left_token.clone(),
          right: binary.right_token.clone(),
          operator: "/".to_string(),
        }),
      }
      BinaryOperator::EqualEqual => Ok(Value::Boolean(self.is_equal(&left, &right))),
      BinaryOperator::BangEqual =>  Ok(Value::Boolean(!self.is_equal(&left, &right))),
    }
  }

  fn is_truthy(&self, value: &Value) -> bool {
    match value {
      Value::Nil => false,
      Value::Boolean(b) => *b,
      _ => true,
    }
  }

  fn is_equal(&self, a: &Value, b: &Value) -> bool {
    match (a,b) {
      (Value::Number(a), Value::Number(b)) => a == b,
      (Value::String(a), Value::String(b)) => a == b,
      (Value::Nil, Value::Nil) => true,
      (Value::Boolean(a), Value::Boolean(b)) => a == b,
      _ => false,
    }
  }

}
