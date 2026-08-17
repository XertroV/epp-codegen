# Error model and diagnostics

Type: grilling
Status: resolved
Blocked by: 01

## Question

What replaces `static mut ERRORS` + `unsafe`?

**Accepted (2026-08-17):** diagnostics module happens (architecture review candidate 3). Sub-questions below settle at implementation time, post-parity.

Sub-questions:

- Diagnostics as returned values (`Vec<Diagnostic>` from parse and emit) — what does a `Diagnostic` carry (message, line number, severity, class/field context)?
- Warn vs error taxonomy: Struct setter access is a warning today; what else warns vs fails?
- Exit-code contract: exit 1 on any error; do warnings still exit 0? What does `run_codegen.py` do with stderr vs exit code (depends on ticket 01)?
- Keep the `colorful` crate for stderr rendering, or plain text?

## Answer

Resolved by grilling, 2026-08-17:

- **Three tiers.** Error (exit 1): constructs the user intended but the tool can't honor — buffer wrapper name not ending in `s`, malformed directive columns. Warn (exit 0, stderr): legal-but-unimplemented access (Struct/Embedded setters) AND silently-skipped invalid field lines (e.g. GameCamera's `0x15` access) — parity-safe since the wrapper never inspects stderr, and the typo footgun becomes visible. Silent: only comment drops with no following directive (established corpus behavior).
- **Diagnostic is a minimal struct** `{ severity: Warn|Error, line: usize, message: String }`; context pre-formatted into the message. Parse and emit each return `Vec<Diagnostic>`; the `static mut ERRORS` global and `unsafe` disappear.
- **Contract**: exit 1 iff ≥1 error diagnostic (and on IO/usage failure); stdout always prints the best-effort file — pure filter, exit code carries status, wrapper discards stdout on failure anyway.
- **`colorful` stays**, confined to the CLI adapter's stderr rendering; diagnostics are plain data.
