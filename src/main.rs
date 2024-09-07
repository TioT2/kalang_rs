pub mod json;
pub mod ast;
pub mod lexer;

fn main() {
    let source = include_str!("../examples/declarations.kl");
    let tokens = lexer::TokenIterator::new(source).collect::<Vec<_>>();
    let ast = ast::Module::parse(&tokens).expect("Error parsing file");
    println!("{:?}", ast);

    // let module_str = include_str!("../examples/declarations.kl");
    // let module = ast::Module::parse(module_str);
    // println!("{:?}", module);

    // let json_str = include_str!("../examples/json/Duck.gltf");
    // let json = json::Json::parse(json_str);
    // println!("{:?}", json);
}
