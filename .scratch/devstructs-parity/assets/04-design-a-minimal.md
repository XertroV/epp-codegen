# Design A — Minimal Interface (design-it-twice, agent 1)

Constraint: 1–3 entry points, max leverage each. Answer: **two free functions and plain data with no behavior.**

## Interface

```rust
// ast.rs — pure data. Two methods total (on Access).
pub type FileAst = Vec<ClassAst>;

pub struct ClassAst {
    pub name: String,
    pub size_check: String,            // verbatim
    pub native_class: Option<String>,
    pub properties: Vec<Property>,
    pub buffers: Vec<BufferProp>,      // separate collection; wrappers after class
    pub comments: Vec<String>,         // class-level, no leading tab
    pub line: usize,
}

pub enum Property {
    Scalar(ScalarProp),
    StructPtr(RefProp),
    Embedded(RefProp),
    Inline(InlineProp),
}

pub struct ScalarProp {
    pub name: String,
    pub ty: String,                    // verbatim
    pub offset: String,                // verbatim
    pub access: Access,
    pub enum_width: Option<String>,    // verbatim "1"|"2"|"4"
    pub rw_type: Option<String>,       // e.g. "MwIdValue"
    pub comments: Vec<String>,
    pub line: usize,
}

/// Shared payload for StructPtr and Embedded: identical columns, different emit template.
pub struct RefProp {
    pub name: String,
    pub ty: String,
    pub offset: String,
    pub access: Access,                // kept only so emit can Warn on 'S'
    pub comments: Vec<String>,
    pub line: usize,
}

pub struct InlineProp { pub code: String, pub comments: Vec<String>, pub line: usize }

pub struct BufferProp {
    pub name: String,
    pub class_name: String,            // must end in 's' (checked in emit)
    pub offset: String,
    pub size: String,
    pub behind_ptr: String,            // lowercased by parse, verbatim after
    pub comments: Vec<String>,
    pub line: usize,
}

pub enum Access { Get, Set, GetSet }
impl Access {
    pub fn readable(self) -> bool { matches!(self, Access::Get | Access::GetSet) }
    pub fn writable(self) -> bool { matches!(self, Access::Set | Access::GetSet) }
}

// parse.rs
pub fn parse(xtoml: &str) -> (FileAst, Vec<Diagnostic>);
// emit.rs
pub fn emit(file: &FileAst) -> (String, Vec<Diagnostic>);
```

- Parse does **no** classification — splits columns, validates `Access`, stores verbatim strings. Invalid access → skip + Warn at parse.
- **All five classification quirks live in emit.rs as private free functions** (`is_enum_ty`, `is_nod_ty`, `nod_handle`, `cast_wrap`, `rw_suffix`). Argument: classification is AngelScript-dialect knowledge, exists only to pick Get*/Set* vocabulary — that vocabulary is emit's entire job. Private fns can't leak.
- Fields `pub(crate)` so only parse can construct.
- Struct/Embedded setter warn + buffer-name error happen in emit.

## Trade-offs (agent's own summary)

- High leverage: two functions total; enum kills the grab-bag bug class at compile time; exhaustive match is the parity checklist.
- Honest costs: invariants conventional not structural; classification testable only through emitted text; `FileAst` alias is a bet; `enum_width: Option<String>` garbage-tolerant (garbage-invisible); no Display escape hatch for debugging.
