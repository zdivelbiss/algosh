#![feature(
    if_let_guard,                   // #51114 <https://github.com/rust-lang/rust/issues/51114>
    let_chains,                     // 53667 <https://github.com/rust-lang/rust/issues/53667>
    pattern,
    anonymous_lifetime_in_impl_trait
)]

extern crate alloc;

pub mod lexer;
pub mod parser;

fn row_src(row: usize, src: &str) -> Option<&str> {
    src.lines().nth(row)
}

fn throw_error(src: &str, msg: &str, token: &lexer::TokenSource, panic: bool) {
    println!("Error: {}", msg);
    if let Some(row_src) = row_src(token.row, src) {
        println!("\t{}", row_src);
        println!(
            "\t{:pad$}{:^underline$}",
            "",
            "",
            pad = token.col,
            underline = token.src.chars().count()
        );
    } else {
        println!("Failed to load the error line. This is a compiler error.");
    }

    if panic {
        loop {
            std::thread::yield_now();
        }
    }
}
