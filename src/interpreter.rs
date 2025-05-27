use crate::expr::{
  Assign, Binary, BinaryOperator, Block, Call, Expr, Grouping, If, Literal, Logical, LogicalOperator, Stmt, Unary, UnaryOperator, VarDecl, Variable, While, FunDecl, Return
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
  CallError {
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
        write!(f, "Run-time error at {}:{}: {}: {}", token.line, token.column, token.lexeme, message)
      }
      RunTimeError::CallError { token, message } => {
        write!(f, "Run-time error at {}:{}: {}: {}", token.line, token.column, token.lexeme, message)
      }
    }
  }
}

trait Callable {
  fn arity(&self) -> usize;
  fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> ExprResult;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
  decl: FunDecl,
  closure: Option<Rc<RefCell<Environment>>>,
}

impl Fun {
  pub fn new(decl: FunDecl,closure: Option<Rc<RefCell<Environment>>> ) -> Self {
    Fun { decl, closure }
  }
}

impl Callable for Fun {
  fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> ExprResult {
    if args.len() != self.decl.params.len() {
      return Err(RunTimeError::CallError {
        token: self.decl.name.clone(),
        message: format!("Expected {} arguments but got {}", self.decl.params.len(), args.len()),
      });
    }
    let prev_env = interpreter.env.clone();
    let env = Rc::new(RefCell::new(Environment::new(self.closure.clone())));
    for (i, param) in self.decl.params.iter().enumerate() {
      env.borrow_mut().define(args[i].clone(), param.clone());
    }

    interpreter.block_stmt(&self.decl.body, env)?;
    interpreter.env = prev_env;
    if let Some(value) = interpreter.return_value.clone() {
      interpreter.return_value = None;
      return Ok(value);
    }
    Ok(Value::Nil)
  }

  fn arity(&self) -> usize {
    self.decl.params.len()
  }
}

impl fmt::Display for Fun {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<function {}>", self.decl.name.lexeme)
  }
}



#[derive(Debug)]
pub struct Interpreter {
  env : Rc<RefCell<Environment>>,
  return_value: Option<Value>,
  locals: Map<Token, usize>,
}

type ExprResult = Result<Value, RunTimeError>;
type StmtResult = Result<(), RunTimeError>;
impl Interpreter {
  pub fn new() -> Self {
    Interpreter {
      env: Rc::new(RefCell::new(Environment::new(None))),
      return_value: None,
      locals: Map::new(),
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
    if self.return_value.is_some() {
      return Ok(());
    }

    match stmt {
      Stmt::Expr(expr) => self.expr_stmt(expr),
      Stmt::Print(expr) => self.print_stmt(expr),
      Stmt::VarDecl(var_decl) => self.var_decl_stmt(var_decl),
      Stmt::Block(block) => self.block_stmt(block, self.env.clone()),
      Stmt::If(if_stmt) => self.if_stmt(if_stmt),
      Stmt::While(while_stmt) => self.while_stmt(while_stmt),
      Stmt::FunDecl(fun_decl) => self.fun_decl_stmt(fun_decl),
      Stmt::Return(return_stmt) => self.ret_stmt(return_stmt),
    }
  }

  fn ret_stmt(&mut self, return_stmt: &Return) -> StmtResult {
    if let Some(value) = &return_stmt.value {
      self.return_value = Some(self.interpret_expr(value)?);
    } else {
      self.return_value = Some(Value::Nil);
    }
    Ok(())
  }

  fn fun_decl_stmt(&mut self, fun_decl: &FunDecl) -> StmtResult {
    let fun = Fun::new(fun_decl.clone(), Some(self.env.clone()));
    self.env.borrow_mut().define(Value::Fun(fun), fun_decl.name.clone());
    Ok(())
  }

  fn while_stmt(&mut self, while_stmt: &While) -> StmtResult {
    while {
      let condition = &self.interpret_expr(&while_stmt.condition)?;
      self.is_truthy(condition)
    }{
      self.interpret_stmt(&while_stmt.body)?;
    }
    Ok(())
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

  fn block_stmt(&mut self, block: &Block, env: Rc<RefCell<Environment>>) -> StmtResult {
    let prev_env = env.clone();
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
      Expr::Call(call) => self.interpret_call(call),
    }
  }

  fn interpret_call(&mut self, call: &Call) -> ExprResult {
    let callee = self.interpret_expr(&call.callee)?;

    let args: Vec<Value> = call.args.iter()
      .map(|arg| self.interpret_expr(arg))
      .collect::<Result<Vec<_>, _>>()?;

    let fun = match callee {
      Value::Fun(fun) => fun,
      _ => {
        return Err(RunTimeError::CallError {
          token: call.paren.clone(),
          message: "Can only call functions".to_string(),
        });
      },
    };

    if fun.arity() != args.len() {
      return Err(RunTimeError::CallError {
        token: call.paren.clone(),
        message: format!("Expected {} arguments but got {}", fun.arity(), args.len()),
      });
    }

    fun.call(self, args)
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

  pub fn resolve(&mut self, token: Token, depth: usize) {
    self.locals.insert(expr, depth);
  }

}
