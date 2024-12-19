mod compiler;
mod scanner;

fn main() {
    let input = "1+1";
    let tokens = scanner::scan(input);
    compiler::compile(&tokens);
}
