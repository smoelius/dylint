#![feature(rustc_private)]
#![feature(let_chains)]
#![warn(unused_extern_crates)]

extern crate rustc_hir;

use std::path::Path;

use anyhow::{Result, anyhow};
use clippy_utils::diagnostics::span_lint;
use match_hir::ErrorKind;
use rustc_hir::HirId;
use rustc_lint::{LateContext, LateLintPass};

mod config;
use config::Config;

mod pattern;
use pattern::Pattern;
use tempfile::TempDir;

use crate::config::lib_path;

dylint_linting::impl_late_lint! {
    /// ### What it does
    ///
    /// ### Why is this bad?
    ///
    /// ### Known problems
    ///
    /// Remove if none.
    ///
    /// ### Example
    ///
    /// ```rust
    /// // example code where a warning is issued
    /// ```
    ///
    /// Use instead:
    ///
    /// ```rust
    /// // example code that does not raise a warning
    /// ```
    pub DISALLOWED_PATTERN,
    Warn,
    "description goes here",
    DisallowedPattern::new()
}

struct DisallowedPattern {
    config: Config<match_hir::Pattern, TempDir>,
}

impl DisallowedPattern {
    pub fn new() -> Self {
        let config: Config<String, String> =
            dylint_linting::config_or_default(env!("CARGO_PKG_NAME"));
        Self {
            config: config.compile(),
        }
    }
}

impl<'tcx> LateLintPass<'tcx> for DisallowedPattern {
    fn check_expr(&mut self, cx: &LateContext<'tcx>, expr: &'tcx rustc_hir::Expr<'tcx>) {
        self.check::<<rustc_hir::Expr as match_hir::HirToSyn>::Syn>(cx, expr.hir_id);
    }
}

impl DisallowedPattern {
    fn check<T>(&self, cx: &LateContext<'_>, hir_id: HirId)
    where
        T: match_hir::Parse + match_hir::Unify,
    {
        for Pattern {
            pattern,
            reason,
            predicate,
            callback,
        } in &self.config.patterns
        {
            match pattern.matches_hir_id::<T>(cx, hir_id) {
                Ok(hir_ids) => {
                    dbg!();
                    if let Some(callback) = callback {
                        assert!(!predicate.is_some());
                        unsafe { call::<bool>(cx, &lib_path(callback.path()), &hir_ids) }.unwrap();
                        return;
                    }
                    if let Some(predicate) = predicate {
                        if !unsafe { call::<bool>(cx, &lib_path(predicate.path()), &hir_ids) }
                            .unwrap()
                        {
                            return;
                        }
                    }
                    let span = cx.tcx.hir().span(hir_id);
                    let msg = reason.clone().unwrap_or(String::from("disallowed pattern"));
                    span_lint(cx, DISALLOWED_PATTERN, span, msg);
                }
                Err(error) => {
                    debug_assert!(matches!(
                        error.kind(),
                        ErrorKind::NoSource | ErrorKind::NoMatch | ErrorKind::NoHirId { .. }
                    ))
                }
            }
        }
    }
}

type Callback<T> = fn(&LateContext<'_>, &[HirId]) -> T;

unsafe fn call<T>(cx: &LateContext, path: &Path, hir_ids: &[HirId]) -> Result<T> {
    let lib = libloading::Library::new(&path)?;
    let func = lib.get::<Callback<T>>(b"callback").map_err(|error| {
        anyhow!(
            "could not find callback in `{}`: {error}",
            path.to_string_lossy()
        )
    })?;
    let result = func(cx, hir_ids);
    Ok(result)
}

#[test]
fn ui() {
    let toml = r##"
[[disallowed_pattern.patterns]]
pattern = "#(_)(#(_))"
type = "Expr"
predicate = """
    |cx: &LateContext<'_>, callee: HirId, arg: HirId| {
        use clippy_utils::is_expr_path_def_path;
        let callee = cx.tcx.hir_node(callee).expect_expr();
        let arg = cx.tcx.hir_node(arg).expect_expr();
        (is_expr_path_def_path(cx, callee, "std::env::remove_var")
            || is_expr_path_def_path(cx, callee, "std::env::set_var")
            || is_expr_path_def_path(cx, callee, "std::env::var"))
            && matches!(arg, ExprKind::Lit(LitKind::Str(symbol, _)))
    }
"""
reason = "referring to an environment variable with a string literal is error prone"
"##;

    dylint_testing::ui::Test::src_base(env!("CARGO_PKG_NAME"), "ui")
        .dylint_toml(toml)
        .run();
}
