#![feature(
    if_let_guard,                   // #51114 <https://github.com/rust-lang/rust/issues/51114>
    let_chains,                     // 53667 <https://github.com/rust-lang/rust/issues/53667>
    pattern,
    anonymous_lifetime_in_impl_trait,
    try_trait_v2,
    once_cell,
    option_result_contains,
    type_alias_impl_trait
)]
#![deny(
    clippy::semicolon_if_nothing_returned,
    clippy::debug_assert_with_mut_call,
    clippy::float_arithmetic
)]
#![warn(clippy::cargo, clippy::pedantic, clippy::undocumented_unsafe_blocks)]
#![allow(
    clippy::cast_lossless,
    clippy::enum_glob_use,
    clippy::inline_always,
    clippy::items_after_statements,
    clippy::must_use_candidate,
    clippy::unreadable_literal,
    clippy::wildcard_imports,
    clippy::wildcard_dependencies,
    dead_code
)]

use ariadne::Report;
use lexer::TokenKind;

pub mod lexer;
pub mod ssa;
// pub mod optimizer;
pub mod parser;
pub mod strings;

pub type Span = logos::Span;

pub enum ErrorKind {
    Unexpected {
        expected: Vec<TokenKind>,
        found: Option<TokenKind>,
    },

    UnclosedDelimiter {
        delimiter: TokenKind,
        delimiter_span: Span,
        expected: TokenKind,
        found: Option<TokenKind>,
    },

    UndeclaredVar {
        var_name: String,
    },
}

pub struct Error {
    span: Span,
    kind: ErrorKind,
    label: Option<&'static str>,
}

impl Error {
    pub const fn unexpected(
        span: Span,
        expected: Vec<TokenKind>,
        found: Option<TokenKind>,
        label: Option<&'static str>,
    ) -> Self {
        Self {
            span,
            kind: ErrorKind::Unexpected { expected, found },
            label,
        }
    }

    pub const fn undeclared_var(span: Span, var_name: String, label: Option<&'static str>) -> Self {
        Self {
            span,
            kind: ErrorKind::UndeclaredVar { var_name },
            label,
        }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn label(&self) -> Option<&'static str> {
        self.label
    }

    pub fn generate_report(&self) -> Report {
        use ariadne::*;

        match self.kind() {
            ErrorKind::Unexpected { expected, found } => {
                let mut msg = String::new();
                if let Some(label) = self.label() {
                    msg.push_str(format!("[{}] ", label).as_str());
                }
                msg.push_str("unexpected input");
                if let Some(found) = found {
                    msg.push_str(format!(", found '{}'", found.to_string()).as_str())
                }

                let mut report = Report::build(ReportKind::Error, (), 8)
                    .with_message(msg)
                    .with_label(
                        Label::new(self.span().clone())
                            .with_message("compiler did not expect this")
                            .with_color(Color::Default),
                    );

                if expected.len() == 1 {
                    report = report.with_note(format!("expected '{}'", expected[0].to_string()))
                } else if expected.len() > 1 {
                    report = report.with_note(format!(
                        "expected one of {}",
                        expected
                            .iter()
                            .map(|t| format!("'{}'", t.to_string()))
                            .collect::<Vec<String>>()
                            .join(", ")
                    ))
                }

                report.finish()
            }

            ErrorKind::UnclosedDelimiter {
                delimiter,
                delimiter_span: _,
                expected,
                found: _,
            } => Report::build(ReportKind::Error, (), 8)
                .with_message("unclosed delimiter")
                .with_label(
                    Label::new(self.span().clone())
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
                .finish(),

            ErrorKind::UndeclaredVar { var_name } => Report::build(ReportKind::Error, (), 8)
                .with_message(format!("use of undeclared variable `{}`", var_name))
                .with_label(Label::new(self.span().clone()))
                .finish(),
        }
    }
}

impl chumsky::Error<TokenKind> for Error {
    type Span = crate::Span;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<TokenKind>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<TokenKind>,
    ) -> Self {
        Self {
            span,
            kind: ErrorKind::Unexpected {
                expected: expected.into_iter().filter_map(|opt| opt).collect(),
                found,
            },
            label: None,
        }
    }

    fn unclosed_delimiter(
        unclosed_span: Self::Span,
        delimiter: TokenKind,
        span: Self::Span,
        expected: TokenKind,
        found: Option<TokenKind>,
    ) -> Self {
        Self {
            span,
            kind: ErrorKind::UnclosedDelimiter {
                delimiter,
                delimiter_span: unclosed_span,
                expected,
                found,
            },
            label: None,
        }
    }

    fn with_label(self, label: Self::Label) -> Self {
        Self {
            span: self.span,
            kind: self.kind,
            label: Some(label),
        }
    }

    fn merge(self, _other: Self) -> Self {
        // FIXME: Actually merge the errors?
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Primitive {
    Int(isize),
    Bool(bool),
    String(intaglio::Symbol),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    // TODO add `Char` type
    Int,
    Bool,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TupleComponent {
    Valued(Primitive),
    Typed(PrimitiveType),
    Inferred,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operator {
    //TODO Not,
    Exp,

    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shr,
    Shl,

    BitXor,
    BitAnd,
    BitOr,

    Eq,
    NotEq,

    Greater,
    GreaterEq,
    Less,
    LessEq,

    Or,
    Xor,
    And,

    Assign,
    Flow,
}
