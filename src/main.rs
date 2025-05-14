mod token;
mod scanner;
mod expr;
mod parser;
mod interpreter;
use std::{io::{self, Write}};

// todo replを実装する
fn main() {

  let args: Vec<String> = std::env::args().collect();
  println!("args: {:?}, len: {}", args, args.len());
  if args.len() == 2 {
    let path = &args[1];
    run_file(path);
  } else {
    println!("Usage: {} <script>", args[0]);
  }
}


fn run_file(path: &str) {
  let source = std::fs::read_to_string(path).expect("Failed to read file");
  run(&source);
}

fn run_prompt() {
  loop {
    print!("> ");
    io::stdout().flush().unwrap();
    let mut line = String::new();
    let bytes = io::stdin().read_line(&mut line).expect("Failed to read line");
    if bytes == 0 {
      println!("exiting...");
      break;
    }
    let line = line.trim();
    println!("You entered: {}", line);
    run(line);
  }
}

fn run(source: &str) {
  let tokens = scanner::Scanner::new(source).scan_tokens();
  println!("tokens: {:?}", tokens);
  if let Some(err) = tokens.as_ref().err() {
    err.iter().for_each(|e| {eprintln!("{}", e);});
    return;
  }

  let ast = parser::Parser::new(tokens.unwrap()).parse();

  if let Some(err) = ast.as_ref().err() {
    eprintln!("{}", err);
    return;
  }
  println!("ast: {:?}", ast);
  let interpreter = interpreter::interpreter::new();
  interpreter.interpret(&ast.unwrap());

}
