#![cfg_attr(not(test), no_std)]
#![feature(
    if_let_guard,                   // #51114 <https://github.com/rust-lang/rust/issues/51114>
)]

extern crate alloc;

pub mod lexer;
