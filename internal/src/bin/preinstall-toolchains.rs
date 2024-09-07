use anyhow::{ensure, Context, Result};
use regex::Regex;
use std::{
    fs::read_to_string,
    io::{BufRead, BufReader},
    process::{Child, Command, Stdio},
};

fn main() -> Result<()> {
    let re = Regex::new(r"\<nightly-[0-9]{4}-[0-9]{2}-[0-9]{2}\>")?;

    let mut ls_files_child = Command::new("git")
        .arg("ls-files")
        .stdout(Stdio::piped())
        .spawn()
        .with_context(|| "Could not spawn `git ls-files`")?;

    let ls_files_stdout = ls_files_child.stdout.take().unwrap();
    let mut toolchains = BufReader::new(ls_files_stdout).lines().try_fold(
        Vec::new(),
        |mut toolchains, result| -> Result<Vec<_>> {
            let path = result.with_context(|| "Could not read from `git ls-files`")?;
            if let Ok(contents) = read_to_string(&path) {
                toolchains.extend(re.find_iter(&contents).map(|m| m.as_str().to_owned()));
            } else {
                eprintln!("Could not read `{path}`");
            }
            Ok(toolchains)
        },
    )?;

    ensure_child_success(ls_files_child)?;

    toolchains.sort();
    toolchains.dedup();

    let rustup_children = toolchains
        .into_iter()
        .map(|toolchain| -> Result<Child> {
            Command::new("rustup")
                .args([
                    "toolchain",
                    "install",
                    &toolchain,
                    "--profile=minimal",
                    "--no-self-update",
                ])
                .spawn()
                .map_err(Into::into)
        })
        .collect::<Result<Vec<_>>>()
        .with_context(|| "Could not spawn `rustup`")?;

    for rustup_child in rustup_children {
        ensure_child_success(rustup_child)?;
    }

    Ok(())
}

fn ensure_child_success(mut child: Child) -> Result<()> {
    let status = child.wait().with_context(|| "Could not wait on child")?;
    ensure!(status.success());
    Ok(())
}
