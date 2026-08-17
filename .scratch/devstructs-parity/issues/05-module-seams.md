# Module seams for the split

Type: grilling
Status: resolved
Blocked by: 03, 04

## Question

Where exactly do the seams go when `main.rs` splits, per the post-parity architecture?

**Accepted (2026-08-17):** all four deepenings land — emit seam first, then parse module, then diagnostics, then CLI adapter. This ticket settles only the remaining seam-placement details below.

AGENT-FIXUP.md proposes `main.rs` (CLI only) / `parse.rs` / `ast.rs` / `emit.rs` / `error.rs`. The architecture review (2026-08-17) ranks: 1. emit seam (`emit(&FileAst) -> String`), 2. parse module, 3. diagnostics, 4. CLI adapter.

Sub-questions:

- Is `ast.rs` its own module or folded into `parse.rs` (one module, internal types)? Deletion test applies.
- Do golden tests migrate from the process seam to the `emit()` seam after the split, or stay at the process seam?
- Visibility discipline: what is `pub` — ideally only `parse`, `emit`, `Diagnostic`, and the AST types tests need.
- Delete `main01.rs` as part of this ticket's implementation.

## Answer

Resolved 2026-08-17 by implication of the other tickets (user-confirmed):

- **`ast.rs` is its own module** — per ticket 04's hybrid design it is the home of `Ty`, `accessor_suffix()`, and the verbatim-payload types; not folded into parse.
- **Goldens stay at the process seam permanently** (ticket 02) — the module split never touches the test harness; no migration to an `emit()` seam.
- **Module layout** (per AGENT-FIXUP.md's recommended shape, confirmed): `main.rs` = CLI adapter only (clap, per ticket 06); `parse.rs` = xtoml → FileAst; `ast.rs` = the types + classification; `emit.rs` = FileAst → AngelScript; `error.rs` = `Diagnostic`.
- **Visibility**: `pub` only what `main.rs` names — `parse`, `emit`, `Diagnostic`, and the AST types that appear in those signatures.
- **`main01.rs` deleted** in the refactor.
- Implementation order within the refactor (accepted candidates, ranked): emit seam first, then parse module, then diagnostics replacing `static mut ERRORS`, then the CLI adapter. Goldens must stay green at every step.
