// Original file: "Rust.hs"

#![feature(box_syntax, box_patterns)]
#![feature(slice_patterns)]

#![allow(unused_parens)]

extern crate num;
#[macro_use]
extern crate maplit;
extern crate parser_c;

#[macro_use]
pub mod support;
pub mod idiomatic;
pub mod ast;
pub mod corrode;

pub use support as corollary_support;
pub use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Language::Rust::AST;

fn main() {
    //TODO
}
