fn main() {
    use std::{io::Read, time::Instant};

    let mut buf = String::new();
    // we naively assume success because I'm lazy.
    std::fs::File::open("example.ash")
        .and_then(|mut file| file.read_to_string(&mut buf))
        .unwrap();

    let start = Instant::now();
    let nodes = parse_input(buf.as_str());
    let end = Instant::now();
    let compile_time = end - start;

    println!("Compiled script in {:.4}s", compile_time.as_secs_f32());
    println!("{:?}", nodes);
}

fn parse_input(input: &str) -> algo::ssa::Nodes {
    let tokens = algo::lexer::lex(input);
    let ast = algo::parser::exprs::parse(tokens).unwrap_or_else(|errs| handle_errors(input, errs));
    let mut nodes = algo::ssa::translate(ast).unwrap_or_else(|errs| handle_errors(input, errs));
    algo::ssa::optimize(&mut nodes);

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
