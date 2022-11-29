extern crate crash;

static LEXER_TEST: &str = "$\"env_var\" = 45 ? { $\"env_var\": 54 } | { $echo: \"failure\" }";

fn main() {
    let lexer = crash::lexer::Lexer {
        chars: LEXER_TEST.chars().peekable()
    };

    for token in lexer {
        println!("{:?}", token);
    }
}
