pub mod json;
pub mod ast;
pub mod lexer;

fn main() {
    // Source
    let source = include_str!("../examples/main.kl");

    // Tokenize file
    let tokens = lexer::TokenIterator::new(source).collect::<Vec<_>>();

    // File -> AST
    let ast = ast::Module::parse(&tokens).expect("Error parsing file");

    println!("{}", ast);

    // Display AST
    // println!("{:?}", ast);

    // let module_str = include_str!("../examples/declarations.kl");
    // let module = ast::Module::parse(module_str);
    // println!("{:?}", module);

    // let json_str = include_str!("../examples/json/Duck.gltf");
    // let json = json::Json::parse(json_str);
    // println!("{:?}", json);
}
