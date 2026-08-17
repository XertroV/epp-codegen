# Design B — Maximum Flexibility (design-it-twice, agent 2)

Constraint: support many use cases and extension. Answer: **richly-typed AST, classification resolved at parse time and encoded in types.**

## Interface (core)

```rust
pub struct Expr(String);   // newtype: verbatim expression, no-normalization made explicit
pub struct Ident(String);  // newtype: identifier, no semantics
pub struct Comment(pub String);

pub struct FileAst { pub classes: Vec<Class> }

pub struct Class {
    pub name: Ident,
    pub size: Expr,                    // verbatim
    pub native_class: Option<Ident>,
    pub members: Vec<Member>,          // in source order
    pub buffers: Vec<Buffer>,          // separate collection (deferred getters)
    pub docs: Vec<Comment>,            // class-level
    pub line: usize,
}

pub struct Member {
    pub docs: Vec<Comment>,
    pub line: usize,
    pub kind: MemberKind,
}

pub enum MemberKind {
    Scalar(ScalarField),
    StructPtr(StructPtrField),
    Embedded(EmbeddedField),
    Inline(InlineAs),
}

pub struct ScalarField {
    pub name: Ident,
    pub ty: Ty,                        // resolved at parse time
    pub offset: Expr,
    pub access: Access,                // { get: bool, set: bool }
    pub rw_override: Option<Ident>,    // only scalars can have it
}

pub struct StructPtrField { pub name: Ident, pub ty: Ident, pub offset: Expr, pub access: Access }
pub struct EmbeddedField  { pub name: Ident, pub ty: Ident, pub offset: Expr, pub access: Access }
pub struct InlineAs(pub String);

pub struct Buffer {
    pub name: Ident,
    pub wrapper: Ident,
    pub offset: Expr,
    pub el_size: Expr,
    pub behind_ptr: bool,
    pub docs: Vec<Comment>,
    pub line: usize,
}

/// THE classification rule, resolved once by parse:
pub enum Ty {
    Scalar(ScalarTy),          // Uint, Int, Float, String, Bool, Other(Ident)
    Nod(Ident),                // Capitalized, no "::"
    Enum(EnumTy),              // Capitalized with "::"; width (1)|(2)|(4), default 4
}
pub struct EnumTy { pub name: Ident, pub width: EnumWidth }
pub enum EnumWidth { W1, W2, W4 }

impl Ty {
    pub fn resolve(raw: &str, width: Option<&str>) -> Ty { /* the one if-chain */ }
    pub fn as_type(&self) -> String;
    pub fn handle(&self) -> &'static str;   // "@" for Nod
}
impl ScalarField {
    pub fn accessor_suffix(&self) -> String;        // rw_override wins; the uint→Uint32 arm
    pub fn setter_takes_const_ref(&self) -> bool;   // as_type == "string" incl. MwIdValue
    pub fn cast_part(&self) -> String;
}
impl Buffer {
    pub fn inner_type(&self) -> Result<Ident, Diagnostic>;  // strip 's' or Error
    pub fn getter_name(&self) -> String;                    // Get{LastSegment}
}

pub fn parse(src: &str) -> (FileAst, Vec<Diagnostic>);
pub fn emit(file: &FileAst) -> (String, Vec<Diagnostic>);
```

- All quirks in `ast.rs` behind `Ty::resolve` + methods; emit contains no capitalization checks, no `::` scans, no Inflector (dependency droppable — explicit match table instead).
- Division defended in review: **ast.rs answers questions about the AST; emit.rs owns every byte of output text** (if a method starts containing format strings, it has crossed the seam).

## Trade-offs (agent's own summary)

- High leverage: MemberKind kills illegal states; resolved Ty makes re-derivation impossible; extension = variant + one parse arm + one emit arm (a *closed* extension point, right for a parity tool).
- Honest costs: ~15 public types (interface width); `Ident` newtype admitted droppable, `Expr` 90% comment-value; `ScalarTy::Other` is speculative forward-compat; typed `behind_ptr: bool` is normalization (behavior-preserving only because input was lowercased anyway). Deliberately NOT proposed (YAGNI): non_exhaustive, visitor trait, directive registry, serde, Unknown catch-all variant (silently tolerating unknown directives is how the Embedded bug happened).
