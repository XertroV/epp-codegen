# AST shape: Property grab-bag to enum

Type: prototype
Status: resolved
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

## Answer

Resolved 2026-08-17 by design-it-twice (three agents: [A-minimal](../assets/04-design-a-minimal.md), [B-flexible](../assets/04-design-b-flexible.md), [C-emit-optimized](../assets/04-design-c-emit-optimized.md)). User picked the recommended **hybrid — "C's spine on A's diet"**:

1. **Variants** (all three agents converged): `Scalar | StructPtr | Embedded | Inline`; buffers a separate collection on the class; verbatim strings for offsets/SIZEs/types; typed `Access`.
2. **`StructPtr`/`Embedded` share one `RefProp` payload with NO `access` field** — `S` access warns at parse; the unimplemented-setter state is unconstructible.
3. **Classification resolved at parse, homed in `ast.rs`**: `enum Ty { Scalar, Nod, Enum { width: EnumWidth } }` + verbatim `as_type` on the scalar. No `Expr`/`Ident` newtypes, no `ScalarTy::Other`.
4. **Exactly one quirk home**: `accessor_suffix()` (rw override → nod → enum width → title-case with `uint`→`Uint32`) + `handle()`. `cast<T>` stays a one-liner in emit so templates read as AngelScript. No precomputed suffix field (avoids denormalizing `Enum{U8}` vs `"Uint8"`).
5. **Buffer constructor validates the `s`-suffix** (error diagnostic at parse, not a side effect in emit) and stores derived `inner`/`get_fn`.
6. **Inflector dropped** for an explicit match table + first-letter-uppercase fallback; goldens hold the line.
7. **Signatures stay per ticket 03**: `parse -> (FileAst, Vec<Diagnostic>)`, `emit -> (String, Vec<Diagnostic>)`. Noted: emit's Vec will usually be empty (all current diagnostics are parse-time) — kept for seam symmetry and because ticket 03 resolved it; not re-litigated.
