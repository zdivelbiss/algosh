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

fn parse_input(input: &str) -> Vec<algo::ssa::Instruction> {
    let tokens = algo::lexer::lex(input);
    println!("{:?}", tokens);
    match algo::parser::parse(tokens).and_then(|ast| {
        println!("{:?}", ast);
        algo::ssa::translate(ast.into_boxed_slice())
    }) {
        Ok((ssa, _)) => ssa,

        Err(errs) => {
            use ariadne::*;

            for err in errs.iter() {
                err.generate_report()
                    .print(Source::from(input))
                    .expect("failed to generate report for error");
            }

            std::process::exit(-1)
        }
    }
}
