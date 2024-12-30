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

fn run_program(path: &str) {
    let source = std::fs::read_to_string(path).unwrap();
    let compiler_result = compiler::compile(&source);
    let chunk = match compiler_result {
        compiler::CompilerResult::CompileError => {
            println!("Compile error");
            return;
        }
        compiler::CompilerResult::Chunk(chunk) => chunk,
    };
    let mut vm = VM::new(chunk);
    vm.run();
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        run_program(&args[1]);
    }
}
