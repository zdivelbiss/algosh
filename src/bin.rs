extern crate crash;

static LEXER_TEST: &str = r#"if$"env_var"~=45{$"env_var":>>~8} else if true { $echo: "failure \" is possible" } else { exit }"#;
static PARSER_TEST: &str = "8 + 2";

fn main() {
    let lexer = crash::lexer::parse(PARSER_TEST);

    for token in lexer {
        println!("{:?}", token);
    }
}
