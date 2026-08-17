//! CLI adapter: argv in, generated AngelScript on stdout, diagnostics on stderr.
//! Exit 1 iff any Error-severity diagnostic (or IO failure); clap exits 2 on argv errors.

mod ast;
mod emit;
mod error;
mod parse;

use clap::Parser;
use colorful::{Color, Colorful};
use error::{Diagnostic, Severity};
use std::{fs, path::PathBuf, process::ExitCode};

const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), " (", env!("GIT_HASH"), ")");

/// Generate AngelScript DevStructs from an .xtoml spec.
#[derive(Parser)]
#[command(name = "epp-codegen", version = VERSION, about)]
struct Cli {
    /// Path to the .xtoml file to compile.
    file_path: PathBuf,
}

fn render(diagnostic: &Diagnostic) -> String {
    let severity = match diagnostic.severity {
        Severity::Warn => "WARN".color(Color::Red3b),
        Severity::Error => "ERROR".color(Color::Red3b),
    };
    format!(
        "[{}]: {} (line {})",
        severity, diagnostic.message, diagnostic.line
    )
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    let input = match fs::read_to_string(&cli.file_path) {
        Ok(input) => input,
        Err(e) => {
            eprintln!("error: cannot read {}: {}", cli.file_path.display(), e);
            return ExitCode::from(1);
        }
    };

    let (ast, mut diagnostics) = parse::parse(&input);
    let (output, emit_diagnostics) = emit::emit(&ast);
    diagnostics.extend(emit_diagnostics);

    print!("{output}");
    for diagnostic in &diagnostics {
        eprintln!("{}", render(diagnostic));
    }

    if diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error)
    {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}
