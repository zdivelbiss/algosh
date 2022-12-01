#![feature(
    if_let_guard,                   // #51114 <https://github.com/rust-lang/rust/issues/51114>
    let_chains,                     // 53667 <https://github.com/rust-lang/rust/issues/53667>
    pattern,
    anonymous_lifetime_in_impl_trait
)]

extern crate alloc;

pub mod lexer;
// pub mod parser;
