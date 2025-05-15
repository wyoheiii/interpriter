use crate::expr::{
  Literal,
  Grouping,
  Binary,
  Unary,
  Expr,
  UnaryOperator,
  BinaryOperator,
  LogicalOperator,
  Stmt,
  Variable,
  VarDecl,
  Assign,
  Block,
  If,
  Logical,
};
use crate::token::Token;
use crate::value::Value;
use crate::environment::Environment;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

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
  UndefinedVariable {
    token: Token,
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
      RunTimeError::UndefinedVariable { token, message } => {
        write!(f, "Run-time error at {}:{}: {}", token.line, token.column, message)
      }
    }
  }
}

#[derive(Debug)]
pub struct Interpreter {
  env : Rc<RefCell<Environment>>,
}

type ExprResult = Result<Value, RunTimeError>;
type StmtResult = Result<(), RunTimeError>;
impl Interpreter {
  pub fn new() -> Self {
    Interpreter {
      env: Rc::new(RefCell::new(Environment::new(None))),
    }
  }

  pub fn interpret(&mut self, stmts: Vec<Stmt>) {
    for stmt in stmts {
      let res = self.interpret_stmt(&stmt);
      if let Err(err) = res {
        eprintln!("{}", err);
        return;
      }
    }
  }

  fn interpret_stmt(&mut self, stmt: &Stmt) -> StmtResult {
    match stmt {
      Stmt::Expr(expr) => self.expr_stmt(expr),
      Stmt::Print(expr) => self.print_stmt(expr),
      Stmt::VarDecl(var_decl) => self.var_decl_stmt(var_decl),
      Stmt::Block(block) => self.block_stmt(block),
      Stmt::If(if_stmt) => self.if_stmt(if_stmt),
    }
  }

  fn if_stmt(&mut self, if_stmt: &If) -> StmtResult {
    let condition = self.interpret_expr(&if_stmt.condition)?;
    if self.is_truthy(&condition) {
      self.interpret_stmt(&if_stmt.then_branch)
    } else if let Some(else_branch) = &if_stmt.else_branch {
      self.interpret_stmt(else_branch)
    } else {
      Ok(())
    }
  }

  fn block_stmt(&mut self, block: &Block) -> StmtResult {
    let prev_env = self.env.clone();
    self.env = Rc::new(RefCell::new(Environment::new(Some(prev_env.clone()))));
    for stmt in &block.stmts {
      let res = self.interpret_stmt(stmt);
      if let Err(err) = res {
        self.env = prev_env;
        return Err(err);
      }

    }
    self.env = prev_env;
    Ok(())
  }

  fn expr_stmt(&mut self, expr: &Expr) -> StmtResult {
    self.interpret_expr(expr)?;
    Ok(())
  }

  fn print_stmt(&mut self, expr: &Expr) -> StmtResult {
    let value = self.interpret_expr(expr)?;
    println!("{}", value);
    Ok(())
  }

  fn var_decl_stmt(&mut self, var_decl: &VarDecl) -> StmtResult {
    let value = if let Some(expr) = &var_decl.initializer {
      self.interpret_expr(expr)?
    } else {
      Value::Nil
    };
    self.env.borrow_mut().define(value, var_decl.name.clone());
    Ok(())
  }

  fn interpret_expr(&mut self, expr: &Expr) -> ExprResult {
    match expr {
      Expr::Literal(literal) => self.interpret_literal(literal),
      Expr::Grouping(grouping) => self.interpret_expr(&grouping.expr),
      Expr::Unary(unary) => self.interpret_unary(unary),
      Expr::Binary(binary) => self.interpret_binary(binary),
      Expr::Variable(variable) => self.interpret_variable(variable),
      Expr::Assign(assign ) => self.interpret_assign(assign),
      Expr::Logical(logical) => self.interpret_logical(logical),
    }
  }

  fn interpret_logical(&mut self, logical: &Logical) -> ExprResult {
    let left = self.interpret_expr(&logical.left)?;
    match logical.operator {
      LogicalOperator::Or => {
        if self.is_truthy(&left) {
          return Ok(left);
        }
      }
      LogicalOperator::And => {
        if !self.is_truthy(&left) {
          return Ok(left);
        }
      }
    }
    self.interpret_expr(&logical.right)
  }

  fn interpret_assign(&mut self, assign: &Assign) -> ExprResult {
    let value = self.interpret_expr(&assign.value)?;
    self.env.borrow_mut().assign(&assign.name, value.clone())?;
    Ok(Value::Nil)
  }

  fn interpret_variable(&mut self, variable: &Variable) -> ExprResult {
    let binding = self.env.borrow_mut().get(&variable.name)?;
    Ok(binding.value.clone())
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

  fn interpret_unary(&mut self, unary: &Unary) -> ExprResult {
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

  fn interpret_binary(&mut self, binary: &Binary) -> ExprResult {
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
