# Design C — Emit-Optimized (design-it-twice, agent 3)

Constraint: make emit trivial; push every parse-time decision into the AST. Answer: **a pre-digested emit plan with verbatim payloads.**

Dividing rule: if a golden diff could be caused by getting a *decision* wrong, pre-resolve it into the AST; if it's a mechanical one-liner from a resolved fact (cast<T>, @ handle), emit may write it inline.

## Interface (core)

```rust
pub struct FileAst { pub classes: Vec<ClassAst> }

pub struct ClassAst {
    pub name: String,
    pub size_check: String,           // verbatim
    pub native_class: Option<String>,
    pub comments: Vec<String>,
    pub members: Vec<Member>,
    pub buffers: Vec<Buffer>,
    pub line: usize,
}

pub enum Member {
    Scalar(Scalar),
    StructPtr(StructPtr),
    Embedded(Embedded),
    Inline(Inline),
}

pub struct Scalar {
    pub name: String,
    pub as_type: String,      // verbatim xtoml type
    pub offset: String,       // verbatim
    pub access: Access,
    pub ty: Ty,               // resolved classification
    pub accessor: Accessor,   // resolved emission facts — the precomputed decision
    pub comments: Vec<String>,
    pub line: usize,
}

pub enum Ty {
    Scalar,
    Nod,
    Enum { width: EnumWidth },   // U8|U16|U32; absent/unknown → U32
}

pub struct Accessor {
    pub suffix: String,          // "Uint32"|"Nod"|"Uint8"|"MwIdValue"(verbatim)…
    pub setter: SetterParam,     // ConstStringIn | Typed
}

pub struct StructPtr {
    pub name: String, pub as_type: String, pub handle: &'static str,
    pub offset: String, pub comments: Vec<String>, pub line: usize,
    // NO access field: 'S' warned away at parse → unimplemented setter unconstructible
}
pub struct Embedded { /* same shape, no access field */ }

pub struct Inline { pub code: String, pub comments: Vec<String>, pub line: usize }

pub struct Buffer {
    pub name: String,
    pub wrapper: String,
    pub offset: String,          // verbatim
    pub size: String,            // verbatim
    pub behind_ptr: String,      // verbatim, lowercased at parse
    pub inner: String,           // wrapper minus 's' — Error diagnostic if absent (at parse)
    pub get_fn: String,          // "Get" + last '_' segment
    pub comments: Vec<String>,
    pub line: usize,
}

pub fn parse(input: &str) -> (FileAst, Vec<Diagnostic>);
pub fn emit(ast: &FileAst) -> (String, Vec<Diagnostic>);
```

- Classification home: `ast.rs` in `Ty::classify` + `Accessor::resolve` — parse stays a tokenizer, emit a printer.
- **Inflector dropped** for an explicit match table (`"uint" => "Uint32"` etc.) — the quirk becomes greppable. Drop only with goldens green.
- All diagnostics harvested at parse (struct/embedded setter warn, buffer-name error). Agent's honest note: **emit's `Vec<Diagnostic>` is vestigial on day one** — emit could be `&FileAst -> String`; kept the tuple for seam symmetry (ticket 03 resolved it that way).

## Trade-offs (agent's own summary)

- High leverage: emit ~80-line printer, every line eyeball-able against goldens; illegal states unrepresentable twice over (enum + dropped access field); buffer-name error moves from side-effecting Display to construction return value.
- Honest costs: AST stores derived facts — an emit-facing view, not a document model (no xtoml round-trip); complexity conserved, relocated to ast.rs (~30-line quirk table); mild denormalization (`Enum{U8}` vs suffix "Uint8" encode one fact twice — goldens catch divergence); new-consumer flexibility is what's traded away.
