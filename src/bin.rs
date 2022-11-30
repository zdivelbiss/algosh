use crash::parser::Parser;

extern crate crash;

static LEXER_TEST: &str = r#"if$"env_var"~=45{$"env_var":>>!856} else if true { $echo: "failure \" is possible" } else { a1d + 1ad }"#;
static PARSER_TEST: &str = "if !1 + 1 = 0 { some } else { none }";

fn main() {
    let lexer = crash::lexer::parse(PARSER_TEST);
    let parser = Parser::new(Box::new(lexer.into_iter()));

    for expr in parser {
        println!("{:?}", expr);
    }
}
