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
        let compiler_result = compiler::compile(&input);
        let chunk = match compiler_result {
            compiler::CompilerResult::CompileError => {
                println!("Compile error");
                continue;
            }
            compiler::CompilerResult::Chunk(chunk) => chunk,
        };
        chunk.disassemble("test");
        let mut vm = VM::new(chunk);
        vm.run();
    }
}

fn main() {
    repl();
}
