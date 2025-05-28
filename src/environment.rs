use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::value::Value;
use crate::token::Token;
use crate::interpreter::RunTimeError;

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
  pub value: Value,
  pub token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
  pub enclosing: Option<Rc<RefCell<Environment>>>,
  pub values: HashMap<String, Binding>,
}

impl Environment {
  pub fn new(enclosing: Option<Rc<RefCell<Environment>>>,
) -> Self {
    Environment {
      enclosing,
      values: HashMap::new(),
    }
  }

  pub fn define(&mut self,  value: Value, token: Token) {
    self.values.insert(token.lexeme.clone(), Binding { value, token });
  }

  pub fn get(&self, token: &Token) -> Result<Binding, RunTimeError> {
    if self.values.contains_key(token.lexeme.as_str()) {
      return Ok(self.values.get(token.lexeme.as_str()).unwrap().clone());
    }

    if let Some(enclosing) = &self.enclosing {
      return enclosing.borrow().get(token);
    }

    Err(RunTimeError::UndefinedVariable {
      token: token.clone(),
      message: format!("Undefined variable '{}'", token.lexeme),
    })
  }

  pub fn assign(&mut self, token: &Token, value: Value) -> Result<(), RunTimeError> {
    if self.values.contains_key(token.lexeme.as_str()) {
      let binding = self.values.get_mut(token.lexeme.as_str()).unwrap();
      binding.value = value;
      return Ok(())
    }

    if let Some(enclosing) = &mut self.enclosing {
      return enclosing.borrow_mut().assign(token, value);
    }

    Err(RunTimeError::UndefinedVariable {
      token: token.clone(),
      message: format!("Undefined variable '{}'", token.lexeme),
    })
  }

  pub fn get_at(env: Rc<RefCell<Self>>, distance: usize, token: &Token) -> Result<Binding, RunTimeError> {
    Self::ancestor(env, distance).borrow().get(token)
  }

  pub fn assign_at(env: Rc<RefCell<Self>>, distance: usize, token: &Token, value: Value) -> Result<(), RunTimeError> {
    Self::ancestor(env, distance).borrow_mut().assign(token, value)
  }

  fn ancestor( env: Rc<RefCell<Self>>, distance: usize) -> Rc<RefCell<Self>> {
    let mut current = env.clone();
    for _ in 0..distance {
      let next = current.borrow().enclosing.as_ref().expect("too much distance").clone();
      current = next;
    }
    current
  }
}
