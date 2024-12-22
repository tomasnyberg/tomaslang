use chunk::Chunk;

mod chunk;
mod compiler;
mod scanner;

fn main() {
    let input = "1+1-1/1*1";
    let tokens = scanner::scan(input);
    let compiled_chunk: Chunk = compiler::compile(&tokens);
    compiled_chunk.disassemble("test");
}
