#![allow(dead_code)]

extern crate algo;

fn main() {
}

fn parse_input(input: &str) -> Vec<algo::ssa::Instruction> {
    match algo::parser::parse(algo::lexer::lex(input)).and_then(|ast| {
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
