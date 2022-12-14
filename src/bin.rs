#![allow(dead_code)]

extern crate algo;

static EXPR_SYNTAX_LEXER_TEST: &str = r#"
    var set: [1, 2, 3, 4, 6, 8, !10];

    // comment

    // ty AddOneFn: { a: Int } => Int;

    foo:
        ~ { a: Int };
        ~ { a: Int, b: Int };
        ~ { a: Int u32, b: Int, c: Int };

    add_one: { a: Int } => a + 1;
    add_one_set: { set: Int, add_one_fn: Int } => set => add_one_fn;
    "#;
static EXPR_PARSER_TEST: &str = "{ a: 1, b: 2, c: 3, d: 8 } (aaa + (b - c)) + d";

fn main() {
    parse_input(EXPR_PARSER_TEST);
}

fn parse_input(input: &str) -> Vec<algo::ssa::Instruction> {
    match algo::parser::parse(algo::lexer::lex(input))
        .and_then(|ast| algo::ssa::translate(ast.into_boxed_slice()))
    {
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
