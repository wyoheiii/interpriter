use crate::expr::{
  Assign, Binary, BinaryOperator, Block, Call, Expr, If, Literal, Logical, LogicalOperator, Stmt, Unary, UnaryOperator, VarDecl, Variable, While, FunDecl, Return, ClassDecl, Get,Set, This, Super,
};
use crate::token::Token;
use crate::value::Value;
use crate::environment::Environment;
use std::cell::RefCell;
use std::collections::HashMap;
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
  AnalysisError {
    token: Token,
    message: String,
  },
  GetError {
    token: Token,
    message: String,
  },
  SetError {
    token: Token,
    message: String,
  },
  ClassError {
    token: Token,
    message: String,
  },
  SuperError {
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
      RunTimeError::AnalysisError { token, message } => {
        write!(f, "Analysis error at {}:{}: {}: {}", token.line, token.column, token.lexeme, message)
      }
      RunTimeError::GetError { token, message } => {
        write!(f, "Run-time error at {}:{}: {}: {}", token.line, token.column, token.lexeme, message)
      }
      RunTimeError::SetError { token, message } => {
        write!(f, "Run-time error at {}:{}: {}: {}", token.line, token.column, token.lexeme, message)
      }
      RunTimeError::ClassError { token, message } => {
        write!(f, "Run-time error at {}:{}: {}: {}", token.line, token.column, token.lexeme, message)
      }
      RunTimeError::SuperError { token, message } => {
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
pub struct Instance {
  class: Class,
  fields: HashMap<String, Value>,
}

impl From<Class> for Instance {
  fn from(class: Class) -> Self {
    Instance::new(class)
  }
}

impl Instance {
  pub fn new(class: Class) -> Self {
    Instance { class, fields: HashMap::new() }
  }

  pub fn get(&self, name: &Token) -> ExprResult {
    if let Some(value) = self.fields.get(&name.lexeme) {
      return Ok(value.clone());
    }

  if let Some(method) = self.class.find_method(&name.lexeme) {
      return Ok(Value::Fun(method.bind(self.clone())));
    }

    Err(RunTimeError::GetError {
      token: name.clone(),
      message: format!("Undefined property '{}'", name.lexeme),
    })
  }

  pub fn set(&mut self, name: Token, value:Value) {
    self.fields.insert(name.lexeme, value);
  }
}

impl fmt::Display for Instance {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} instance", self.class.name.lexeme)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
  name: Token,
  super_class: Option<Box<Class>>,
  methods: HashMap<String, Fun>,
}

impl Class {
  pub fn new(name: Token, super_class: Option<Box<Class>> , methods: HashMap<String, Fun>) -> Self {
    Class { name, super_class , methods }
  }

  pub fn find_method(&self, name: &str) -> Option<Fun> {
    if let Some(method) = self.methods.get(name) {
      return Some(method.clone());
    }
    if let Some(super_class) = &self.super_class {
      return super_class.find_method(name);
    }
    None
  }
}

impl Callable for Class {
  fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> ExprResult {
    let instance = Instance::from(self.clone());

    let init = self.find_method("init");
    if init.is_some() {
      init.unwrap().bind(instance.clone()).call(interpreter, args)?;
    }

    Ok(Value::Instance(instance))
  }

  fn arity(&self) -> usize {
    let init = self.find_method("init");
    match init {
      Some(fun) => fun.arity(),
      None => 0,
    }
  }
}

impl fmt::Display for Class {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name.lexeme)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
  decl: FunDecl,
  closure: Option<Rc<RefCell<Environment>>>,
  is_init: bool,
}

impl Fun {
  pub fn new(decl: FunDecl,closure: Option<Rc<RefCell<Environment>>>, is_init: bool) -> Self {
    Fun { decl, closure, is_init }
  }

  pub fn bind(&self, instance: Instance) -> Fun {
    let env = Rc::new(RefCell::new(Environment::new(self.closure.clone())));
    env.borrow_mut().define(Value::Instance(instance), Token::this_dummy());
    Fun {
      decl: self.decl.clone(),
      closure: Some(env),
      is_init: self.is_init,
    }
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

      if value == Value::Nil && self.is_init {
        return Ok(Environment::get_at(self.closure.as_ref().unwrap().clone(), 0, &Token::this_dummy())?.value.clone());
      }

      return Ok(value);
    }

    if self.is_init {
      return Ok(Environment::get_at(self.closure.as_ref().unwrap().clone(), 0, &Token::this_dummy())?.value.clone());
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
  globals: Rc<RefCell<Environment>>,
  env : Rc<RefCell<Environment>>,
  return_value: Option<Value>,
  locals: HashMap<Token, usize>,
}

type ExprResult = Result<Value, RunTimeError>;
type StmtResult = Result<(), RunTimeError>;
impl Interpreter {
  pub fn new() -> Self {
    let global = Rc::new(RefCell::new(Environment::new(None)));
    Interpreter {
      env: global.clone(),
      globals: global,
      return_value: None,
      locals: HashMap::new(),
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
      Stmt::ClassDecl(class_decl) => self.class_decl_stmt(class_decl),
    }
  }

  fn class_decl_stmt(&mut self, class: &ClassDecl) -> StmtResult {

    let super_class;

    if let Some(super_class_expr) = &class.super_class {
      let c = self.interpret_expr(super_class_expr)?;
      if let Value::Class(class) = c {
        super_class = Some(Box::new(class));
      } else {
        return Err(RunTimeError::ClassError {
          token: class.name.clone(),
          message: "Superclass must be a class".to_string(),
        });
      }
    } else {
      super_class = None;
    }

    self.env.borrow_mut().define(Value::Nil, class.name.clone());

    if super_class.is_some() {
      self.env = Rc::new(RefCell::new(Environment::new(Some(self.env.clone()))));
      // let name = class.name.clone();
      self.env.borrow_mut().define(Value::Class(*super_class.clone().unwrap()), Token::super_dummy());
    }


    let mut methods: HashMap<String, Fun> = HashMap::new();
    for method in &class.methods {
      let fun = Fun::new(method.clone(), Some(self.env.clone()), method.name.lexeme == "init");
      methods.insert(method.name.lexeme.clone(), fun);
    }

    let class = Class::new(class.name.clone(), super_class.clone(), methods);

    if super_class.is_some() {
      let enclosing = self.env.borrow().enclosing.clone();
      self.env = enclosing.unwrap();
    }


    self.env.borrow_mut().assign(&class.name, Value::Class(class.clone()))?;

    Ok(())
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
    let fun = Fun::new(fun_decl.clone(), Some(self.env.clone()), false);
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
      Expr::Get(get) => self.interpret_get(get),
      Expr::Set(set) => self.interpret_set(set),
      Expr::This(this) => self.interpret_this(this),
      Expr::Super(s) => self.interpret_super(s),
    }
  }

  fn interpret_super(&mut self, _super: &Super) -> ExprResult {
    let distance = self.locals.get(&_super.keyword)
      .ok_or(RunTimeError::SuperError {
        token: _super.keyword.clone(),
        message: "Cannot use 'super' outside of a class".to_string(),
      })?;

    let super_class = Environment::get_at(self.env.clone(), *distance, &_super.keyword)?.value;
    let val;
    let method = match super_class {
      Value::Class(class) => {
        val = if let Value::Instance(i) = Environment::get_at(self.env.clone(), *distance - 1, &Token::this_dummy())?.value {
          i
        } else {
          return Err(RunTimeError::SuperError {
            token: _super.keyword.clone(),
            message: "Cannot use 'super' outside of a class".to_string(),
          });
        };

        class.find_method(&_super.method.lexeme)
          .ok_or(RunTimeError::SuperError {
            token: _super.method.clone(),
            message: format!("Undefined method '{}'", _super.method.lexeme),
          })?
        }
      _ => {
        return Err(RunTimeError::SuperError {
          token: _super.keyword.clone(),
          message: "Superclass must be a class".to_string(),
        });
      },
    };

    Ok(Value::Fun(method.bind(val)))
  }

  fn interpret_this(&mut self, this: &This) -> ExprResult {
    self.look_up_variable(&this.keyword)
  }

  fn interpret_set(&mut self, set: &Set) -> ExprResult {
    let obj = self.interpret_expr(&set.object)?;


    if let Value::Instance(mut instance) = obj {
      let val = self.interpret_expr(&set.value)?;
      instance.set(set.name.clone(), val.clone());
      Ok(val)
    } else {
      Err(RunTimeError::SetError {
        token: set.name.clone(),
        message: "Can only set properties on instances".to_string(),
      })
    }
  }

  fn interpret_get(&mut self, get: &Get) -> ExprResult {
    let val = self.interpret_expr(&get.object)?;

    match val {
      Value::Instance(instance) => {
        // let i = instance.get(&get.name)?;
        Ok(instance.get(&get.name)?)
      }
      _ => Err(RunTimeError::AnalysisError {
        token: get.name.clone(),
        message: "Can only access properties on instances".to_string(),
      }),
    }
  }

  fn interpret_call(&mut self, call: &Call) -> ExprResult {
    let callee = self.interpret_expr(&call.callee)?;

    let args: Vec<Value> = call.args.iter()
      .map(|arg| self.interpret_expr(arg))
      .collect::<Result<Vec<_>, _>>()?;

    let callable:Box<dyn Callable> = match callee {
      Value::Fun(fun) => Box::new(fun),
      Value::Class(class) => Box::new(class),

      _ => {
        return Err(RunTimeError::CallError {
          token: call.paren.clone(),
          message: "Can only call functions or methods".to_string(),
        });
      },
    };

    if callable.arity() != args.len() {
      return Err(RunTimeError::CallError {
        token: call.paren.clone(),
        message: format!("Expected {} arguments but got {}", callable.arity(), args.len()),
      });
    }

    callable.call(self, args)
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
    if let Some(distance) = self.locals.get(&assign.name) {
      Environment::assign_at(self.env.clone(), *distance, &assign.name, value.clone())?;
    } else {
      self.globals.borrow_mut().assign(&assign.name, value.clone())?;
    }
    Ok(value)
  }

  fn interpret_variable(&mut self, variable: &Variable) -> ExprResult {
    Ok(self.look_up_variable(&variable.name)?)
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

  pub fn resolve(&mut self, token: &Token, depth: usize) {

    self.locals.insert(token.clone(), depth);
  }

  fn look_up_variable(&mut self, name: &Token) -> ExprResult {
    if let Some(distance) = self.locals.get(name) {
      return Environment::get_at(self.env.clone(), *distance, name).map(|binding| binding.value.clone());
    }

    Ok(self.globals.borrow().get(name).map(|binding| binding.value)?)
  }

}
