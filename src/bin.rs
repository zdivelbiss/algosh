#![allow(dead_code)]

extern crate crash;

static LEXER_TEST: &str = r#"if$"env_var"~=45{$"env_var":>>!856} else if true { $echo: "failure \" is possible" } else { a1d + 1ad }"#;
static PARSER_TEST: &str = r#"if randomvar = 5 { $echo: ["blah", "blah", "blah"] } else if randomvar = 6 { $"TEST": [!2845, 1999990, "asdf"] } else { none }"#;
static ARRAY_TEST: &str = r#"["blah0", !127, "blah2"]"#;
static ENV_VARIABLE_TEST: &str = r#"$"TEST": !128"#;
static EXPR_SYNTAX_LEXER_TEST: &str = r#"
    const set: [1, 2, 3, 4, 6, 8, !10];

    lazy add_one: |a: Int| a + 1;
    lazy add_one_set: |set: [Int]| set => add_one;
    "#;

fn main() {
    let lexer = crash::lexer::lexer(EXPR_SYNTAX_LEXER_TEST);
    // let parser = crash::parser::Parser::new(lexer);

    for a in lexer {
        println!("{:?}", a);
    }
}
