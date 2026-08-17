# Module seams for the split

Type: grilling
Status: open
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
