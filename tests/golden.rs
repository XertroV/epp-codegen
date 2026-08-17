use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn collect_fixtures(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("read fixture dir") {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            collect_fixtures(&path, out);
        } else if path.extension().map_or(false, |e| e == "xtoml") {
            out.push(path);
        }
    }
    out.sort();
}

/// Context around the first differing line between expected and actual output.
fn first_diff_context(expected: &str, actual: &str) -> String {
    let exp_lines: Vec<&str> = expected.lines().collect();
    let act_lines: Vec<&str> = actual.lines().collect();
    let common = exp_lines.len().min(act_lines.len());
    let idx = (0..common)
        .find(|&i| exp_lines[i] != act_lines[i])
        .unwrap_or(common);

    let mut msg = String::new();
    if idx >= exp_lines.len() && idx >= act_lines.len() {
        // Same lines, but different trailing bytes (e.g. final newline count).
        msg.push_str(&format!(
            "outputs share all lines but differ in bytes (expected {} bytes, got {} bytes)\n",
            expected.len(),
            actual.len()
        ));
        return msg;
    }
    msg.push_str(&format!("first difference at line {}:\n", idx + 1));
    let start = idx.saturating_sub(2);
    let end = (idx + 3).min(exp_lines.len().max(act_lines.len()));
    for i in start..end {
        let e = exp_lines.get(i).copied().unwrap_or("<missing>");
        let a = act_lines.get(i).copied().unwrap_or("<missing>");
        if e == a {
            msg.push_str(&format!("  {:>4} | {}\n", i + 1, e));
        } else {
            msg.push_str(&format!("- {:>4} | {}\n", i + 1, e));
            msg.push_str(&format!("+ {:>4} | {}\n", i + 1, a));
        }
    }
    msg
}

#[test]
fn golden_fixtures() {
    let bin = env!("CARGO_BIN_EXE_epp-codegen");
    let fixtures_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");

    let mut fixtures = Vec::new();
    collect_fixtures(&fixtures_root, &mut fixtures);
    assert!(!fixtures.is_empty(), "no .xtoml fixtures found");

    let mut failures: Vec<String> = Vec::new();

    for fixture in &fixtures {
        let rel = fixture.strip_prefix(&fixtures_root).unwrap();
        let expected_path = fixture.with_file_name(format!(
            "{}.expected.as",
            fixture.file_stem().unwrap().to_string_lossy()
        ));
        let expected = match fs::read(&expected_path) {
            Ok(b) => b,
            Err(e) => {
                failures.push(format!(
                    "{}: cannot read expected file {}: {e}",
                    rel.display(),
                    expected_path.display()
                ));
                continue;
            }
        };

        let output = match Command::new(bin).arg(fixture).output() {
            Ok(o) => o,
            Err(e) => {
                failures.push(format!("{}: failed to run binary: {e}", rel.display()));
                continue;
            }
        };

        if !output.status.success() {
            failures.push(format!(
                "{}: exit status {}, stderr:\n{}",
                rel.display(),
                output.status,
                String::from_utf8_lossy(&output.stderr)
            ));
            continue;
        }

        if output.stdout != expected {
            let diff = first_diff_context(
                &String::from_utf8_lossy(&expected),
                &String::from_utf8_lossy(&output.stdout),
            );
            failures.push(format!("{}: stdout mismatch\n{diff}", rel.display()));
        }
    }

    if !failures.is_empty() {
        panic!(
            "{}/{} fixtures failed:\n\n{}",
            failures.len(),
            fixtures.len(),
            failures.join("\n\n")
        );
    }
}
