pub mod json;
pub mod ast;
pub mod lexer;

fn q(i: impl Copy) {
    std::hint::black_box(i);
    std::hint::black_box(i);
    std::hint::black_box(i);
}

fn main() {
    let source = include_str!("../examples/main.kl");
    let tokens = lexer::TokenIterator::new(source).collect::<Vec<_>>();
    println!("{:?}", tokens);

    // let module_str = include_str!("../examples/declarations.kl");
    // let module = ast::Module::parse(module_str);
    // println!("{:?}", module);

    // let json_str = include_str!("../examples/json/Duck.gltf");
    // let json = json::Json::parse(json_str);
    // println!("{:?}", json);
}
