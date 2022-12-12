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

pub mod lexer;
pub mod linearizer;
pub mod parser;
pub mod strings;

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Int(isize),
    Bool(bool),
    String(intaglio::Symbol),
}

#[derive(Debug, PartialEq)]
pub enum PrimitiveType {
    // TODO add `Char` type
    Int,
    Bool,
    String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Shr,
    Shl,
    Xor,
    BitAnd,
    BitOr,

    Eq,
    NegEq,

    Greater,
    GreaterEq,
    Less,
    LessEq,
}

impl From<&'_ str> for Operator {
    fn from(value: &'_ str) -> Self {
        match value {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            ">>" => Self::Shr,
            "<<" => Self::Shl,
            "^" => Self::Xor,
            "&" => Self::BitAnd,
            "|" => Self::BitOr,

            _ => unimplemented!(),
        }
    }
}

impl std::str::FromStr for Operator {
    type Err = <Self as TryFrom<&'static str>>::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Condition {
    Eq,
    NegEq,
    Or,
    And,
    Greater,
    GreaterEq,
    Less,
    LessEq,
}

impl From<&'_ str> for Condition {
    fn from(value: &'_ str) -> Self {
        match value {
            "=" => Self::Eq,
            "!=" => Self::NegEq,
            ">" => Self::Greater,
            ">=" => Self::GreaterEq,
            "<" => Self::Less,
            "<=" => Self::LessEq,
            "or" => Self::Or,
            "and" => Self::And,

            _ => unimplemented!(),
        }
    }
}

impl std::str::FromStr for Condition {
    type Err = <Self as TryFrom<&'static str>>::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Control {
    Assign,
    Insert,
}
impl From<&'_ str> for Control {
    fn from(value: &'_ str) -> Self {
        match value {
            ":" => Self::Assign,
            "=>" => Self::Insert,

            _ => unimplemented!(),
        }
    }
}

impl std::str::FromStr for Control {
    type Err = <Self as TryFrom<&'static str>>::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}
