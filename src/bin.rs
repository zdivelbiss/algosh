// use crash::parser::Parser;

extern crate crash;

static LEXER_TEST: &str = r#"if$"env_var"~=45{$"env_var":>>!856} else if true { $echo: "failure \" is possible" } else { a1d + 1ad }"#;
static PARSER_TEST: &str = "if { some } else if false = false { none }";

fn main() {
    let lexer = crash::lexer::lexer(PARSER_TEST);
    let parser = crash::parser::Parser::new(lexer);

    for a in parser {
        println!("{:#?}", a);
    }
}
