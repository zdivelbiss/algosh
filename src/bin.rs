#![allow(dead_code)]

extern crate algo;

static LEXER_TEST: &str = r#"if$"env_var"~=45{$"env_var":>>!856} else if true { $echo: "failure \" is possible" } else { a1d + 1ad }"#;
static PARSER_TEST: &str = r#"if randomvar = 5 { $echo: ["blah", "blah", "blah"] } else if randomvar = 6 { $"TEST": [!2845, 1999990, "asdf"] } else { none }"#;
static ARRAY_TEST: &str = r#"["blah0", !127, "blah2"]"#;
static ENV_VARIABLE_TEST: &str = r#"$"TEST": !128"#;
static EXPR_SYNTAX_LEXER_TEST: &str = r#"
    var set: [1, 2, 3, 4, 6, 8, !10];

    // comment

    // ty AddOneFn: { a: Int } => Int;

    var add_one: { a: Int } => a + 1;
    var add_one_set: { set: Int, add_one_fn: AddOneFn } => set => add_one_fn;
    "#;
static EXPR_PARSER_TEST: &str = "(1 + (3 - 1)) + (8 / 5)";
static EXPR_PARSER_TEST_ARRAY: &str = "lazy add_one: |a: Int| 1 + 1;";
static CHUMSKY_TEST: &str = "{set: Int} => 1 + 2 - 8";

fn main() {
    let lexer = algo::lexer::lexer(EXPR_SYNTAX_LEXER_TEST);
    let exprs = algo::parser::parse(lexer);

    // println!("{errs:?}");
    println!("{exprs:#?}");
}
