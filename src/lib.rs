#![feature(concat_idents)]
#![feature(type_ascription)]
#[macro_use]
pub mod macros;
pub mod exec;
pub mod metadata;
pub mod util;

extern crate llvm_sys as llvm;
extern crate rustc_hash;
