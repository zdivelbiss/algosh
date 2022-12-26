fn main() {
    use std::{io::Read, time::Instant};

    let mut buf = String::new();
    // we naively assume success because I'm lazy.
    std::fs::File::open("example.ash")
        .and_then(|mut file| file.read_to_string(&mut buf))
        .unwrap();

    let start = Instant::now();

    let input = buf.as_str();
    let tokens = algo::lexer::lex(input);
    let exprs = algo::parser::parse(tokens).unwrap_or_else(|errs| handle_errors(input, errs));
    let typed_ast =
        algo::types::type_exprs(exprs).unwrap_or_else(|errs| handle_errors(input, errs));

    let end = Instant::now();
    let compile_time = end - start;

    println!("Compiled script in {:.4}s", compile_time.as_secs_f32());
    println!("{:?}", typed_ast);
}

fn handle_errors(src: &str, errs: Vec<algo::Error>) -> ! {
    for err in errs {
        err.generate_report()
            .eprint(ariadne::Source::from(src))
            .unwrap();
    }

    std::process::exit(-1)
}
