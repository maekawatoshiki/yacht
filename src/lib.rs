#![feature(concat_idents)]
#[macro_use]
pub mod macros;
pub mod exec;
pub mod metadata;

extern crate llvm_sys as llvm;
extern crate rustc_hash;
