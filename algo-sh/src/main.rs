#![allow(dead_code)]

use std::io::Read;

fn main() {
    let mut buf = String::new();
    // we naively assume success because I'm lazy.
    std::fs::File::open("example.ash")
        .and_then(|mut file| file.read_to_string(&mut buf))
        .unwrap();

    parse_input(buf.as_str());
}

fn parse_input(input: &str) -> algo::ssa::Nodes {
    let tokens = algo::lexer::lex(input);
    let ast = algo::parser::parse(tokens).unwrap_or_else(|errs| handle_errors(input, errs));
    let nodes = algo::ssa::translate(ast).unwrap_or_else(|errs| handle_errors(input, errs));

    use algo::ssa::Node;


    nodes
}

fn handle_errors(src: &str, errs: Vec<algo::Error>) -> ! {
    for err in errs {
        err.generate_report()
            .eprint(ariadne::Source::from(src))
            .unwrap();
    }

    std::process::exit(-1)
}
