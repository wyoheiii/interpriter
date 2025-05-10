mod token;
mod scanner;
mod expr;
mod parser;
use std::io::{self, Write};

// todo replを実装する
fn main() {
  let ex = expr::Expr::Binary(
    expr::Binary {
      left: Box::new(expr::Expr::Literal(expr::Literal::Number(1.0))),
      operator: expr::BinaryOperator::Plus,
      right: Box::new(expr::Expr::Literal(expr::Literal::Number(2.0))),
    },
  );

  let p = expr::PrettyPrinter;
  println!("{}", ex.accept(&p));
  
  // let args: Vec<String> = std::env::args().collect();
  // if args.len() == 1 {
  //   let path = &args[1];
  //   run_file(path);
  // } else {
  //   run_prompt();
  // }
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
  let mut scanner = scanner::Scanner::new(source);
  // todo err handling
  let tokens = scanner.scan_tokens().unwrap();

  for token in tokens {
    println!("{:?}", token);
  }

  // match reader.read() {
  //   Ok(_) => println!("Read successfully"),
  //   Err(e) => println!("Error reading: {}", e),
  // }
}

// todo: エラートレイトを実装する
fn err(line: usize,column: usize , where_: String, msg: &str) {
  eprintln!("[line \"{} \": column \"{}\"] {} : {}", line, column, where_ , msg);
  // todo 視覚的にわかりやすいように場所も表示する
}
