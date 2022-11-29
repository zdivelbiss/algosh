extern crate crash;

static LEXER_TEST: &str = "if $\"env_var\" = 45 { $\"env_var\": 54 } else { $echo: \"failure\" }";



fn main() {
    let lexer = crash::lexer::parse(LEXER_TEST);

    for token in lexer {
        println!("{:?}", token);
    }
}
