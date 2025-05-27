use crate::interpreter::{Interpreter, RunTimeError};
use crate::expr::{
	Assign, Block, FunDecl, Stmt, Variable, If, While, VarDecl, Expr, Binary, Call, Logical, Return
};
use crate::token::Token;
use std::collections::HashMap;

#[derive(Debug)]
struct Scope(HashMap<String, bool>);

impl Scope {
	pub fn new() -> Self {
		Scope(HashMap::new())
	}

	pub fn declare(&mut self, name: String) {
		self.0.insert(name, false);
	}

	pub fn define(&mut self, name: String) {
		if let Some(entry) = self.0.get_mut(&name) {
			*entry = true;
		}
	}

	pub fn is_defined(&self, name: &str) -> bool {
		self.0.get(name).cloned().unwrap_or(false)
	}
}



pub struct Resolver<'a> {
	interpreter: &'a mut Interpreter,
	scopes: Vec<Scope>,
}

enum ResolverError {
	UndefinedVariable{
		message: String,
		token: Token,
	},
}

type ResolveResult<T> = Result<T, RunTimeError>;

impl <'a> Resolver<'a> {
	pub fn new(interpreter: &'a mut Interpreter) -> Self {
		Resolver { 
			interpreter,
			scopes: Vec::new()
		}
	}

	fn resolve(&mut self, stmts: &[Stmt])-> ResolveResult<()> {
		stmts.iter().map( |s| 
			self.resolve_stmt(s)
		).collect::<Result<_, _>>()?;
		Ok(())
	}
	
	fn resolve_expr(&mut self, expr: &Expr) -> ResolveResult<()> {
		match expr {
			Expr::Variable(var) => self.variable_expr(var)?,
			Expr::Assign(a) => self.assign_expr(a)?,
			Expr::Binary(b) => self.binary_expr(b)?,
			Expr::Call(call) => self.call_expr(call)?,
			Expr::Grouping(g) => self.resolve_expr(&g.expr)?,
			Expr::Logical(l) => self.logical_expr(l)?,
			Expr::Unary(u) => self.resolve_expr(&u.right)?,
			Expr::Literal(_) => {}, // Literals do not need resolution
		}
		Ok(())
	}
	
	fn resolve_stmt(&mut self, stmt: &Stmt)-> ResolveResult<()> {
		match stmt {
			Stmt::Block(block) => self.block_stmt(block),
			Stmt::VarDecl(var_decl) => self.var_decl_stmt(var_decl),
			Stmt::FunDecl(f) => self.fun_decl_stmt(f),
			Stmt::If(if_stmt) => self.if_stmt(if_stmt)?,
			Stmt::While(while_stmt) => self.while_stmt(while_stmt)?,
			Stmt::Return(return_stmt) => self.return_stmt(return_stmt)?,
			Stmt::Expr(e) | Stmt::Print(e) => self.resolve_expr(e)?,
		}
		Ok(())
	}
	
	pub fn block_stmt(&mut self, block: &Block) {
		self.begin_scope();
		self.resolve(&block.stmts);
		self.end_scope();
	}

	fn var_decl_stmt(&mut self, var_decl: &VarDecl) {
		self.declare(&var_decl.name.lexeme);
		if let Some(init) = &var_decl.initializer {
			self.resolve_expr(init);
		}
		self.define(&var_decl.name.lexeme);
	}

	fn fun_decl_stmt(&mut self, fun_decl: &FunDecl) {
		self.declare(&fun_decl.name.lexeme);
		self.define(&fun_decl.name.lexeme);
		self.begin_scope();
		self.resolve_fun(fun_decl);
	}

	fn if_stmt(&mut self, if_stmt: &If)-> ResolveResult<()> {
		self.resolve_expr(&if_stmt.condition)?;
		self.resolve_stmt(&if_stmt.then_branch)?;
		if let Some(else_branch) = &if_stmt.else_branch {
			self.resolve_stmt(else_branch)?;
		}
		Ok(())
	}

	fn return_stmt(&mut self, return_stmt: &Return)-> ResolveResult<()> {
		if let Some(value) = &return_stmt.value {
			self.resolve_expr(value)?;
		}
		Ok(())
	}

	fn while_stmt(&mut self, while_stmt: &While)-> ResolveResult<()> {
		self.resolve_expr(&while_stmt.condition)?;
		self.resolve_stmt(&while_stmt.body)?;
		Ok(())
	}

	fn declare(&mut self, name: &str) {
		if let Some(scope) = self.scopes.last_mut() {
			scope.declare(name.to_string());
		}
	}

	fn variable_expr(&mut self, var: &Variable) -> ResolveResult<()> {
		if !self.scopes.is_empty() 
		&& !self.scopes.last().unwrap().is_defined(&var.name.lexeme) {
			return Err(RunTimeError::AnalysisError {
				message: "cant read local variable in its own initializer".to_string(),
				token: var.name.clone(),
			});
		}

		self.resolve_local(&var.name);
		Ok(())
	}

	fn assign_expr(&mut self, assign: &Assign) -> ResolveResult<()> {
		self.resolve_expr(&assign.value)?;
		self.resolve_local(&assign.name);
		Ok(())
	}

	fn binary_expr(&mut self, binary: &Binary) -> ResolveResult<()> {
		self.resolve_expr(&binary.left)?;
		self.resolve_expr(&binary.right)?;
		Ok(())
	}

	fn logical_expr(&mut self, logical: &Logical) -> ResolveResult<()> {
		self.resolve_expr(&logical.left)?;
		self.resolve_expr(&logical.right)?;
		Ok(())
	}

	fn call_expr(&mut self, call: &Call) -> ResolveResult<()> {
		self.resolve_expr(&call.callee)?;

		for arg in &call.args {
			self.resolve_expr(arg)?;
		}

		Ok(())
	}

	fn define(&mut self, name: &str) {
		if let Some(scope) = self.scopes.last_mut() {
			scope.define(name.to_string());
		}
	}

	fn resolve_local(&mut self, name: &Token) {
		for (i, scope) in self.scopes.iter().enumerate().rev() {
			if scope.is_defined(&name.lexeme) {
				self.interpreter.resolve(name, i);
				return;
			}
		}
	}

	fn resolve_fun(&mut self, fun_decl: &FunDecl) {
		self.begin_scope();

		for param in &fun_decl.params {
			self.declare(&param.lexeme);
			self.define(&param.lexeme);
		}
		self.resolve(&fun_decl.body.stmts);

		self.end_scope();
	}

	fn begin_scope(&mut self) {
		self.scopes.push(Scope::new());
	}

	fn end_scope(&mut self) {
		self.scopes.pop();
	}
}

