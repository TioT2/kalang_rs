//! Main file

extern crate kalang;

fn main() {
    // Source
    let source = include_str!("../examples/main.kl");

    // Source -> Tokens
    let tokens = kalang::lexer::TokenIterator::new(source).collect::<Vec<_>>();

    // Tokens -> AST
    let ast = kalang::ast::Module::parse(&tokens).expect("Error parsing file");

    // AST -> Output
    println!("{}", ast);
}

// file main.rs