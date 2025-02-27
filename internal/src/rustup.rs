use crate::{CommandExt, env};
use anyhow::{Result, anyhow};
use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
    process::Command,
};

// smoelius: Should this be merged into `CommandExt`?
pub trait SanitizeEnvironment {
    fn sanitize_environment(&mut self) -> &mut Self;
}

impl SanitizeEnvironment for Command {
    fn sanitize_environment(&mut self) -> &mut Self {
        self.env_remove(env::CARGO);
        self.env_remove(env::RUSTC);
        self.env_remove(env::RUSTUP_TOOLCHAIN);
        self
    }
}

// smoelius: Consider carefully whether you need to call this function! In most cases, the toolchain
// you want is not the one returned by rustup.
pub fn active_toolchain(path: &Path) -> Result<String> {
    let output = Command::new("rustup")
        .sanitize_environment()
        .current_dir(path)
        .args(["show", "active-toolchain"])
        .logged_output(true)?;
    let stdout = std::str::from_utf8(&output.stdout)?;
    stdout
        .split_once(' ')
        .map(|(s, _)| s.to_owned())
        .ok_or_else(|| anyhow!("Could not determine active toolchain"))
}

pub fn toolchain_path(path: &Path) -> Result<PathBuf> {
    let output = Command::new("rustup")
        .sanitize_environment()
        .current_dir(path)
        .args(["which", "rustc"])
        .logged_output(true)?;
    let stdout = std::str::from_utf8(&output.stdout)?;
    let path = PathBuf::from(stdout);
    // smoelius: `path` should end with `/bin/rustc`.
    path.ancestors()
        .nth(2)
        .map(Into::into)
        .ok_or_else(|| anyhow!("Could not get ancestor"))
}

pub fn is_rustc<T: AsRef<OsStr> + ?Sized>(arg: &T) -> bool {
    Path::new(arg).file_stem() == Some(OsStr::new("rustc"))
}

#[test]
fn rustc_is_rustc() {
    assert!(is_rustc("rustc"));
}
