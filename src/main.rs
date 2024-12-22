use chunk::Chunk;
use std::io::{self, Write};

mod chunk;
mod compiler;
mod scanner;

fn repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let tokens = scanner::scan(&input);
        let compiled_chunk: Chunk = compiler::compile(&tokens);
        compiled_chunk.disassemble("test");
    }
}

fn main() {
    repl();
}
