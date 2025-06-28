use crate::Pattern;
use anyhow::{Result, ensure};
use serde::{Deserialize, Deserializer};
use std::{
    env::consts,
    fs::{create_dir, write},
    path::{Path, PathBuf},
};
use syn::spanned::Spanned;
use tempfile::TempDir;

#[derive(Default)]
pub struct Config<T, U> {
    pub patterns: Vec<Pattern<T, U>>,
}

#[derive(Deserialize)]
struct StringPatterns {
    patterns: Vec<Pattern<String, String>>,
}

impl<'de> Deserialize<'de> for Config<String, String> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let StringPatterns { patterns } = StringPatterns::deserialize(deserializer)?;
        Ok(Config { patterns })
    }
}

impl Config<String, String> {
    /// Panic
    ///
    /// Panics if a pattern cannot be parsed, or if predicate/callback cannot be compiled.
    pub fn compile(self) -> Config<match_hir::Pattern, TempDir> {
        let patterns = self
            .patterns
            .into_iter()
            .map(|pattern| {
                let Pattern {
                    pattern,
                    reason,
                    predicate,
                    callback,
                } = pattern;
                let pattern = pattern.parse::<match_hir::Pattern>().unwrap();
                let predicate = predicate.map(|predicate| compile(&predicate, true).unwrap());
                let callback = callback.map(|callback| compile(&callback, false).unwrap());
                Pattern {
                    pattern,
                    reason,
                    predicate,
                    callback,
                }
            })
            .collect();
        Config { patterns }
    }
}

const RUST_TOOLCHAIN: &str = include_str!("../rust-toolchain");

fn compile(callback: &str, is_predicate: bool) -> Result<TempDir> {
    let (inputs, body) = parse_callback(callback)?;
    let output = if is_predicate { " -> bool" } else { "" };

    let tempdir = TempDir::new()?;

    write(tempdir.path().join("rust-toolchain"), RUST_TOOLCHAIN)?;

    write(
        tempdir.path().join("Cargo.toml"),
        r#"
[package]
name = "callback"
version = "0.1.0"
edition = "2024"
publish = false

[lib]
crate-type = ["cdylib"]
"#,
    )?;

    create_dir(tempdir.path().join("src"))?;

    write(
        tempdir.path().join("src/lib.rs"),
        format!(
            r#"
fn callback({inputs}){output} {body}
"#
        ),
    )?;

    Ok(tempdir)
}

fn parse_callback(callback: &str) -> Result<(&str, &str)> {
    let closure = syn::parse_str::<syn::ExprClosure>(callback)?;
    let syn::ExprClosure {
        attrs,
        lifetimes,
        constness,
        movability,
        asyncness,
        capture,
        or1_token: _,
        inputs,
        or2_token: _,
        output,
        body,
    } = closure;
    ensure!(attrs.is_empty());
    ensure!(lifetimes.is_none());
    ensure!(constness.is_none());
    ensure!(movability.is_none());
    ensure!(asyncness.is_none());
    ensure!(capture.is_none());
    let inputs = &callback[inputs.span().byte_range()];
    ensure!(matches!(output, syn::ReturnType::Default));
    let body = &callback[body.span().byte_range()];
    Ok((inputs, body))
}

pub fn lib_path(path: &Path) -> PathBuf {
    path.join("target/release/callback")
        .with_extension(consts::DLL_EXTENSION)
}
