pub mod json;
pub mod ast;
pub mod lexer;

fn main() {
    // Source
    let source = include_str!("../examples/main.kl");

    // Source -> Tokens
    let tokens = lexer::TokenIterator::new(source).collect::<Vec<_>>();

    // Tokens -> AST
    let ast = ast::Module::parse(&tokens).expect("Error parsing file");

    println!("{}", ast);
}
