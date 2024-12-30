use std::io::{self, Write};
use vm::{interpret, VM};

mod chunk;
mod compiler;
mod scanner;
mod vm;

fn run_program(source: &str) {
    interpret(source);
}

fn repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        run_program(&input);
    }
}

fn run_file(path: &str) {
    let source = std::fs::read_to_string(path).unwrap();
    run_program(&source);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        run_file(&args[1]);
    }
}
