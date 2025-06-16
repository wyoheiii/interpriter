mod token;
mod scanner;
mod expr;
mod parser;
mod interpreter;
mod environment;
mod value;
mod resolver;

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

fn run(source: &str) {
  let tokens = scanner::Scanner::new(source).scan_tokens();

  if let Some(err) = tokens.as_ref().err() {
    err.iter().for_each(|e| {eprintln!("{}", e);});
    return;
  }

  let ast = parser::Parser::new(tokens.unwrap()).parse();

  if let Some(err) = ast.as_ref().err() {
    err.iter().for_each(|e| {eprintln!("{}", e);});
    return;
  }

  let mut it = interpreter::Interpreter::new();
  let res = resolver::Resolver::new(&mut it).resolve(&ast.clone().unwrap());
  if let Err(err) = res {
    eprintln!("{}", err);
    return;
  }

  it.interpret(ast.unwrap());

}
