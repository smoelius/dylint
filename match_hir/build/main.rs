#![feature(let_chains)]

use std::env::var;

mod common;
mod hir;
mod visit;

fn main() {
    let out_dir = var("OUT_DIR").unwrap();

    hir::emit_impls(&out_dir);
    visit::emit_impls(&out_dir);

    println!("cargo:rerun-if-changed=assets/hir.rs");
    println!("cargo:rerun-if-changed=assets/visit.rs");
}
