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
    let ast = algo::parser::parse(tokens).unwrap_or_else(|errs| handle_errors(input, errs));

    // TLE, or `Top Level Expression`
    let (defs, tles) =
        ast.into_iter()
            .fold((Vec::new(), Vec::new()), |(mut defs, mut tles), expr| {
                if expr.0.is_def() {
                    defs.push(expr);
                } else {
                    tles.push(expr);
                }

                (defs, tles)
            });

    let mut tles_iter = tles.into_iter();
    let tle = {
        match tles_iter.len() {
            0 => handle_errors(input, vec![algo::Error::no_top_level_expr()]),
            1 => tles_iter.next().unwrap(),
            _ => handle_errors(
                input,
                tles_iter
                    .map(|expr: algo::parser::HeapExpr| {
                        algo::Error::general(
                            expr.1.clone(),
                            "cannot have multiple top-level expressions",
                            Some("parse_input.tle_check"),
                        )
                    })
                    .collect(),
            ),
        }
    };

    let a = algo::ssa::translate(tle.as_ref(), defs).unwrap();
    println!("{:?}", a);

    a.0
}

fn handle_errors(src: &str, errs: Vec<algo::Error>) -> ! {
    for err in errs {
        err.generate_report()
            .eprint(ariadne::Source::from(src))
            .unwrap();
    }

    std::process::exit(-1)
}
