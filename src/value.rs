use crate::interpreter::Class;
use crate::interpreter::Fun;
use crate::interpreter::Instance;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
  Number(f64),
  String(String),
  Boolean(bool),
  Fun(Fun),
  Class(Class),
  Instance(Instance),
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
          Value::Fun(f) => f.to_string(),
          Value::Instance(i) => i.to_string(),
          Value::Class(c) => c.to_string(),
          Value::Nil => "nil".to_string(),

      })
  }
}