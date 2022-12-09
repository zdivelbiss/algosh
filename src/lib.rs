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

#[allow(dead_code)]
fn row_src(row: usize, src: &str) -> Option<&str> {
    src.lines().nth(row)
}

#[allow(dead_code)]
fn throw_error(msg: &str, src: Option<&str>, token: Option<lexer::TokenSource>) -> ! {
    println!("error: {msg}");
    if let Some(token) = token
        && let Some(src) = src
        && let Some(row_src) = row_src(token.row, src)
    {
        // use a 1's based value for the line number (clearer meani)
        let row_num = (token.row + 1).to_string();
        let row_num_digits = row_num.chars().count();
        // print padding line
        println!(" {: >row_num_digits$} |", "");
        // print row string from source
        println!(" {row_num: >row_num_digits$} | {row_src}");
        // print token underline
        println!(" {: >row_num_digits$} | {: >pad$}", "", "^".repeat(token.src.chars().count()), pad = (token.col + row_num_digits + 3));
    } else {
        println!("Failed to load the error line.");
        println!("This is a compiler error.");
    }

    std::process::exit(-1)
}
