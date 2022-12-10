#![allow(dead_code)]

extern crate algo;

static EXPR_SYNTAX_LEXER_TEST: &str = r#"
    var set: [1, 2, 3, 4, 6, 8, !10];

    // comment

    // ty AddOneFn: { a: Int } => Int;

    var add_one: { a: Int  => a + 1;
    var add_one_set: { set: Int, add_one_fn: Int  => set => add_one_fn;
    "#;
static EXPR_PARSER_TEST: &str = "(1 + (3 - 1)) + (8 / 5";

fn main() {
    let lexer = algo::lexer::lexer(EXPR_PARSER_TEST);
    let (exprs, errs) = algo::parser::parse(lexer.clone());

    if errs.is_empty() {
        println!("{:?}", exprs);
    } else {
        use algo::lexer::TokenKind;
        use ariadne::*;

        for err in errs.iter() {
            let expected = err
                .expected()
                .filter_map(|o| o.as_ref().map(|kind| format!("'{}'", <&str>::from(kind))))
                .collect::<Vec<String>>()
                .join(" ");

            match err.reason() {
                chumsky::error::SimpleReason::Unexpected => {
                    let mut report = Report::build(ReportKind::Error, (), 8)
                        .with_message("unexpected token")
                        .with_label(
                            Label::new(err.span().clone())
                                .with_message("compiler did not expect this token")
                                .with_color(Color::Default),
                        );

                    if !expected.is_empty() {
                        report = report
                            .with_help(format!("suggested tokens: {}", expected.fg(Color::Green)));
                    }

                    report
                        .finish()
                        .print(Source::from(lexer.src()))
                        .expect("compiler failed to generate error report");
                }

                chumsky::error::SimpleReason::Unclosed { span, delimiter } => {
                    Report::build(ReportKind::Error, (), 8)
                        .with_message("unclosed delimiter")
                        .with_label(
                            Label::new(span.clone())
                                .with_message("expected delimiter for this block")
                                .with_color(Color::Default),
                        )
                        .with_help(format!(
                            "try inserting {} at the end of the {}",
                            expected.fg(Color::Green),
                            match delimiter {
                                TokenKind::TupleOpen => "tuple declaration",
                                TokenKind::ArrayOpen => "array declaration",
                                TokenKind::GroupOpen => "grouping",
                                _ => "code block",
                            }
                        ))
                        .finish()
                        .print(Source::from(lexer.src()))
                        .expect("compiler failed to generate error report");
                }

                chumsky::error::SimpleReason::Custom(_) => todo!(),
            }
        }

        println!(
            "{} failed to compile source due to ({}) errors",
            "Error:".fg(Color::Red),
            errs.len()
        );

        std::process::exit(-1)
    }
}
