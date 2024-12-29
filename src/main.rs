use chunk::Chunk;
use std::io::{self, Write};
use vm::VM;

mod chunk;
mod compiler;
mod scanner;
mod vm;

fn repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let tokens = scanner::scan(&input);
        let compiled_chunk: Chunk = compiler::compile(&tokens);
        compiled_chunk.disassemble("test");
        let mut vm = VM::new(compiled_chunk);
        vm.run();
    }
}

fn main() {
    repl();
}
