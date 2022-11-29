extern crate crash;

static LEXER_TEST: &str = r#"if$"env_var"~=45{$"env_var":>>~8} else if true { $echo: "failure \" is possible" } else { exit }"#;



fn main() {
    let lexer = crash::lexer::parse(LEXER_TEST);

    for token in lexer {
        println!("{:?}", token);
    }
}
