use crate::interpreter::Interpreter;
use crate::expr::{
	Assign, Block, FunDecl, Stmt, Variable, If, While, VarDecl, Expr, Binary, Call, Logical, Return, ClassDecl, Get, Set, This, Super
};
use crate::token::Token;
use std::collections::HashMap;

#[derive(Debug)]
struct Scope(HashMap<String, bool>);

impl Scope {
	pub fn new() -> Self {
		Scope(HashMap::new())
	}

	pub fn declare(&mut self, name: &str) {
		self.0.insert(name.to_string(), false);
	}

	pub fn define(&mut self, name: &str) {
		if let Some(entry) = self.0.get_mut(name) {
			*entry = true;
		}
	}

	pub fn status(&self, name: &str) -> Option<bool> {
    self.0.get(name).cloned()
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum FunctionType {
  None,
  Function,
  Init,
  Method,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ClassType {
  None,
  Class,
  SubClass,
}

pub struct Resolver<'a> {
	interpreter: &'a mut Interpreter,
	scopes: Vec<Scope>,
  current_func: FunctionType,
  current_class: ClassType,
}

pub struct ResolveError {
  message: String,
  token: Token,
}

use std::fmt;
impl fmt::Display for ResolveError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ResolveError: {} at {}:{}", self.message, self.token.line, self.token.column)
  }
}

type ResolveResult = Result<(), ResolveError>;

impl <'a> Resolver<'a> {
	pub fn new(interpreter: &'a mut Interpreter) -> Self {
		Resolver {
			interpreter,
			scopes: Vec::new(),
      current_func: FunctionType::None,
      current_class: ClassType::None,
		}
	}

	pub fn resolve(&mut self, stmts: &[Stmt])-> ResolveResult {
		stmts.iter().map( |s|
			self.resolve_stmt(s)
		).collect::<Result<(), ResolveError>>()?;
		Ok(())
	}

	fn resolve_expr(&mut self, expr: &Expr) -> ResolveResult {
		match expr {
			Expr::Variable(var) => self.variable_expr(var)?,
			Expr::Assign(a) => self.assign_expr(a)?,
			Expr::Binary(b) => self.binary_expr(b)?,
			Expr::Call(call) => self.call_expr(call)?,
			Expr::Grouping(g) => self.resolve_expr(&g.expr)?,
			Expr::Logical(l) => self.logical_expr(l)?,
			Expr::Unary(u) => self.resolve_expr(&u.right)?,
      Expr::Get(g) => self.resolve_get(g)?,
      Expr::Set(s) => self.resolve_set(s)?,
      Expr::This(t) => self.resolve_this(t)?,
      Expr::Super(s) => self.resolve_super(s)?,
			Expr::Literal(_) => {}, // Literals do not need resolution
		}
		Ok(())
	}

	fn resolve_stmt(&mut self, stmt: &Stmt)-> ResolveResult {
		match stmt {
			Stmt::Block(block) => self.block_stmt(block)?,
			Stmt::VarDecl(var_decl) => self.var_decl_stmt(var_decl)?,
			Stmt::FunDecl(f) => self.fun_decl_stmt(f)?,
			Stmt::If(if_stmt) => self.if_stmt(if_stmt)?,
			Stmt::While(while_stmt) => self.while_stmt(while_stmt)?,
			Stmt::Return(return_stmt) => self.return_stmt(return_stmt)?,
			Stmt::Expr(e) | Stmt::Print(e) => self.resolve_expr(e)?,
      Stmt::ClassDecl(class) => self.class_decl_stmt(class)?,
		}
		Ok(())
	}

  fn resolve_super(&mut self, super_class: &Super) -> ResolveResult {
    if self.current_class == ClassType::None {
      return Err(ResolveError {
        message: "cannot use 'super' outside of a class".to_string(),
        token: super_class.keyword.clone(),
      });
    } else if self.current_class != ClassType::SubClass {
      return Err(ResolveError {
        message: "cannot use 'super' in a class with no superclass".to_string(),
        token: super_class.keyword.clone(),
      });
    }

    self.resolve_local(&super_class.keyword);
    Ok(())
  }

  fn resolve_this(&mut self, this: &This) -> ResolveResult {
    if self.current_class == ClassType::None {
      return Err(ResolveError {
        message: "cannot use 'this' outside of a class".to_string(),
        token: this.keyword.clone(),
      });
    }
    self.resolve_local(&this.keyword);
    Ok(())
  }

  fn resolve_set(&mut self, set: &Set) -> ResolveResult {
    self.resolve_expr(&set.object)?;
    self.resolve_expr(&set.value)?;
    Ok(())
  }

  fn resolve_get(&mut self, get: &Get) -> ResolveResult {
    self.resolve_expr(&get.object)?;
    Ok(())
  }

  fn class_decl_stmt(&mut self, class:& ClassDecl) -> ResolveResult {
    let enclosing = self.current_class;
    self.current_class = ClassType::Class;
    self.declare(&class.name)?;
    self.define(&class.name);

    if let Some(Expr::Variable(super_class))= &class.super_class {
      if class.name.lexeme == super_class.name.lexeme {
        return Err(ResolveError {
          message: "a class cannot inherit from itself".to_string(),
          token: super_class.name.clone(),
        });
      }
      self.current_class = ClassType::SubClass;
      self.resolve_expr(&Expr::Variable(super_class.clone()))?;
    }

    if class.super_class.is_some() {
      self.begin_scope();
      self.scopes.last_mut().unwrap().declare("super");
      self.scopes.last_mut().unwrap().define("super");
    }

    self.begin_scope();
    self.scopes.last_mut().unwrap().declare("this");
    self.scopes.last_mut().unwrap().define("this");

    for m in &class.methods {
      let decl = if m.name.lexeme == "init" {
        FunctionType::Init
      } else {
        FunctionType::Method
      };

      self.resolve_fun(m, decl)?;
    }

    self.end_scope();

    if class.super_class.is_some() {
      self.end_scope();
    }

    self.current_class = enclosing;
    Ok(())
  }

	fn block_stmt(&mut self, block: &Block)-> ResolveResult {
		self.begin_scope();
		self.resolve(&block.stmts)?;
		self.end_scope();

    Ok(())
	}

	fn var_decl_stmt(&mut self, var_decl: &VarDecl)-> ResolveResult {
		self.declare(&var_decl.name)?;
		if let Some(init) = &var_decl.initializer {
			self.resolve_expr(init)?;
		}
		self.define(&var_decl.name);

    Ok(())
	}

	fn fun_decl_stmt(&mut self, fun_decl: &FunDecl)-> ResolveResult {
		self.declare(&fun_decl.name)?;
		self.define(&fun_decl.name);

		self.resolve_fun(fun_decl, FunctionType::Function)?;

    Ok(())
	}

	fn if_stmt(&mut self, if_stmt: &If)-> ResolveResult {
		self.resolve_expr(&if_stmt.condition)?;
		self.resolve_stmt(&if_stmt.then_branch)?;
		if let Some(else_branch) = &if_stmt.else_branch {
			self.resolve_stmt(else_branch)?;
		}
		Ok(())
	}

	fn return_stmt(&mut self, return_stmt: &Return)-> ResolveResult {
    if self.current_func == FunctionType::None {
      return Err(ResolveError {
        message: "cannot return from top-level code".to_string(),
        token: return_stmt.keyword.clone(),
      });
    }

		if let Some(value) = &return_stmt.value {
      if self.current_func == FunctionType::Init {
        return Err(ResolveError {
          message: "cannot return a value from an initializer".to_string(),
          token: return_stmt.keyword.clone(),
        });
      }

			self.resolve_expr(value)?;
		}
		Ok(())
	}

	fn while_stmt(&mut self, while_stmt: &While)-> ResolveResult {
		self.resolve_expr(&while_stmt.condition)?;
		self.resolve_stmt(&while_stmt.body)?;
		Ok(())
	}

	fn declare(&mut self, name: &Token)-> ResolveResult {
		if let Some(scope) = self.scopes.last_mut() {
      if let Some(_) = scope.status(&name.lexeme) {
        return Err(ResolveError {
          message: format!("variable '{}' already declared in this scope", name.lexeme),
          token: name.clone(),
        });
      }
			scope.declare(&name.lexeme);
		}
    Ok(())
	}

	fn variable_expr(&mut self, var: &Variable) -> ResolveResult {
		if let Some(scope) = self.scopes.last() {
      if let Some(false) = scope.status(&var.name.lexeme) {
        return Err(ResolveError {
          message: "cant read local variable in its own initializer".to_string(),
          token: var.name.clone(),
        });
      }
    }

    self.resolve_local(&var.name);
    Ok(())
	}

	fn assign_expr(&mut self, assign: &Assign) -> ResolveResult {
		self.resolve_expr(&assign.value)?;
		self.resolve_local(&assign.name);
		Ok(())
	}

	fn binary_expr(&mut self, binary: &Binary) -> ResolveResult {
		self.resolve_expr(&binary.left)?;
		self.resolve_expr(&binary.right)?;
		Ok(())
	}

	fn logical_expr(&mut self, logical: &Logical) -> ResolveResult {
		self.resolve_expr(&logical.left)?;
		self.resolve_expr(&logical.right)?;
		Ok(())
	}

	fn call_expr(&mut self, call: &Call) -> ResolveResult {
		self.resolve_expr(&call.callee)?;

		for arg in &call.args {
			self.resolve_expr(arg)?;
		}

		Ok(())
	}

	fn define(&mut self, name: &Token) {
		if let Some(scope) = self.scopes.last_mut() {
			scope.define(&name.lexeme);
		}
	}

	fn resolve_local(&mut self, name: &Token) {
    let len = self.scopes.len();
    if len == 0 {
      return;
    }
    for i in (0..=(len - 1)).rev() {
      if self.scopes.get(i).unwrap().status(&name.lexeme).is_some() {
        self.interpreter.resolve(name, len - 1 - i);
        return;
      }
    }
	}

	fn resolve_fun(&mut self, fun_decl: &FunDecl, ty: FunctionType)-> ResolveResult {
		self.begin_scope();

    let enclosing= self.current_func;
    self.current_func = ty;

		for param in &fun_decl.params {
			self.declare(&param)?;
			self.define(&param);
		}
		self.resolve(&fun_decl.body.stmts)?;

		self.end_scope();

    self.current_func = enclosing;
    Ok(())
	}

	fn begin_scope(&mut self) {
		self.scopes.push(Scope::new());
	}

	fn end_scope(&mut self) {
		self.scopes.pop();
	}
}
