#![feature(
    if_let_guard,                   // #51114 <https://github.com/rust-lang/rust/issues/51114>
    let_chains,                     // 53667 <https://github.com/rust-lang/rust/issues/53667>
    pattern,
    anonymous_lifetime_in_impl_trait,
    try_trait_v2,
    once_cell,
    option_result_contains
)]

pub mod lexer;
pub mod minifier;
pub mod parser;
pub mod strings;

fn row_src(row: usize, src: &str) -> Option<&str> {
    src.lines().nth(row)
}

fn throw_error(msg: &str, src: Option<&str>, token: Option<lexer::TokenSource>) -> ! {
    println!("error: {}", msg);
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
        println!(" {: >row_num_digits$} | {}", row_num, row_src);
        // print token underline
        println!(" {: >row_num_digits$} | {: >pad$}", "", "^".repeat(token.src.chars().count()), pad = (token.col + row_num_digits + 3));
    } else {
        println!("Failed to load the error line.");
        println!("This is a compiler error.")
    }

    std::process::exit(-1)
}
