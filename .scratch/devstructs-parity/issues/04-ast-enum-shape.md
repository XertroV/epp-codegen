# AST shape: Property grab-bag to enum

Type: prototype
Status: open
Blocked by: 02

## Question

What is the AST that parse produces and emit consumes — the interface both refactor seams share?

**Accepted (2026-08-17):** the grab-bag becomes an enum as part of the parse-module deepening (candidates 1+2). Sub-questions below settle at implementation time, post-parity.

Today `Property` is a 9-field struct with flags (`is_struct`, `inline_definition`, `rw_type`, `opt_size`) where illegal combinations are representable. AGENT-FIXUP.md suggests `Scalar | StructPtr | Embedded | Inline` (+ `Buffer` alongside).

Sub-questions:

- Exact variant set and what each carries (does comment attachment live on every variant?).
- Where do type-classification helpers go (`is_nod`, `is_enum`, `enum_type_uint_size`, `capitalized_type` — including the fragile `uint` → `Uint32` Inflector special case)?
- Does the AST own offset/size *strings* verbatim (needed for `0x40+0x20` passthrough) or parse them?
- Candidate for a design-it-twice prototype: two agents sketch the enum, compare on depth and on how the emit match arms read.
