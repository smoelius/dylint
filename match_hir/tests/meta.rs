use assert_cmd::Command;
use std::{fs::read_to_string, path::Path};

#[test]
fn lint() {
    const PREFIX_AND_SUFFIX_ARGS: [(&[&str], &[&str]); 2] =
        [(&["clippy"], &[]), (&["dylint", "--all", "--"], &[])];
    for (prefix_args, suffix_args) in PREFIX_AND_SUFFIX_ARGS {
        let mut command = Command::new("cargo");
        command.args(prefix_args);
        command.args(["--all-targets"]);
        command.args(suffix_args);
        command.env("DYLINT_RUSTFLAGS", "--deny warnings");
        command.assert().success();
    }
}

#[test]
fn self_reflective_match_exact() {
    self_reflective_match(false);
}

#[test]
fn self_reflective_match_wildcard() {
    self_reflective_match(true);
}

fn self_reflective_match(wildcard: bool) {
    Command::new("cargo")
        .arg("build")
        .current_dir("reflective_match")
        .assert()
        .success();

    let mut command = Command::new("cargo");
    command.args(["dylint", "--path", "reflective_match"]);
    if wildcard {
        command.env("WILDCARD", "1");
    }
    let assert = command.assert();
    assert!(assert.get_output().status.success());
    let stderr_expected = read_to_string(Path::new("tests").join(format!(
        "reflective_match_{}.stderr",
        if wildcard { "wildcard" } else { "exact" }
    )))
    .unwrap();
    let stderr_actual = std::str::from_utf8(&assert.get_output().stderr).unwrap();
    assert!(
        stderr_actual.contains(&stderr_expected),
        "unexpected stderr: ```\n{stderr_actual}\n```"
    );
}
