//! Diagnostics: plain data produced by parse and emit, rendered by the CLI adapter.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Warn,
    Error,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub line: usize,
    pub message: String,
}

impl Diagnostic {
    pub fn warn(line: usize, message: String) -> Self {
        Diagnostic {
            severity: Severity::Warn,
            line,
            message,
        }
    }

    pub fn error(line: usize, message: String) -> Self {
        Diagnostic {
            severity: Severity::Error,
            line,
            message,
        }
    }
}
