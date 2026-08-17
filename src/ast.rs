//! AST types and type classification: the interface parse produces and emit consumes.
//! Offsets, SIZEs, and type names are verbatim strings; classification is resolved at
//! parse and homed here (`Ty`, `accessor_suffix()`, `handle()`).

use crate::error::Diagnostic;

pub struct FileAst {
    pub classes: Vec<ClassAst>,
}

pub struct ClassAst {
    pub name: String,
    pub size_check: String, // verbatim
    pub native_class: Option<String>,
    pub members: Vec<Member>,
    pub buffers: Vec<Buffer>,
    pub comments: Vec<String>,
}

pub enum Member {
    Scalar(Scalar),
    StructPtr(RefProp),
    Embedded(RefProp),
    Inline(InlineProp),
}

pub struct Scalar {
    pub name: String,
    pub as_type: String, // verbatim xtoml type
    pub offset: String,  // verbatim
    pub access: Access,
    pub ty: Ty,
    pub rw_type: Option<String>, // verbatim Get*/Set* vocabulary override, e.g. MwIdValue
    pub comments: Vec<String>,
}

/// Shared payload for StructPtr and Embedded: identical columns, different emit template.
/// No access field: `S` is warned away at parse, so the unimplemented-setter state is
/// unconstructible.
pub struct RefProp {
    pub name: String,
    pub as_type: String,
    pub offset: String,
    pub ty: Ty,
    pub comments: Vec<String>,
}

pub struct InlineProp {
    pub code: String,
}

pub struct Buffer {
    pub name: String,
    pub wrapper: String,
    pub offset: String,    // verbatim
    pub size: String,      // verbatim
    pub behind_ptr: String, // verbatim, lowercased at parse
    pub inner: String,     // wrapper minus trailing 's'
    pub get_fn: String,    // "Get" + last '_' segment of inner (whole inner if no '_')
    pub comments: Vec<String>,
}

impl Buffer {
    /// Validates the wrapper's `s` suffix and derives `inner`/`get_fn`. A missing suffix
    /// is an Error diagnostic; the buffer is still built best-effort so emit can continue.
    pub fn new(
        name: String,
        wrapper: String,
        offset: String,
        size: String,
        behind_ptr: String,
        comments: Vec<String>,
        line: usize,
    ) -> (Buffer, Option<Diagnostic>) {
        let (inner, diagnostic) = match wrapper.strip_suffix('s') {
            Some(inner) => (inner.to_string(), None),
            None => (
                wrapper.clone(),
                Some(Diagnostic::error(
                    line,
                    "Buffer types must end in 's'".to_string(),
                )),
            ),
        };
        let get_fn = format!("Get{}", inner.split('_').next_back().unwrap_or(""));
        (
            Buffer {
                name,
                wrapper,
                offset,
                size,
                behind_ptr,
                inner,
                get_fn,
                comments,
            },
            diagnostic,
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Access {
    Get,
    Set,
    GetSet,
}

impl Access {
    pub fn parse(s: &str) -> Option<Access> {
        match s {
            "G" => Some(Access::Get),
            "S" => Some(Access::Set),
            "GS" => Some(Access::GetSet),
            _ => None,
        }
    }

    pub fn readable(self) -> bool {
        matches!(self, Access::Get | Access::GetSet)
    }

    pub fn writable(self) -> bool {
        matches!(self, Access::Set | Access::GetSet)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EnumWidth {
    U8,
    U16,
    U32,
}

impl EnumWidth {
    /// xtoml enum size column: "1" -> u8, "2" -> u16, absent or anything else -> u32.
    pub fn from_size(opt_size: Option<&str>) -> EnumWidth {
        match opt_size {
            Some("1") => EnumWidth::U8,
            Some("2") => EnumWidth::U16,
            _ => EnumWidth::U32,
        }
    }

    pub fn suffix(self) -> &'static str {
        match self {
            EnumWidth::U8 => "Uint8",
            EnumWidth::U16 => "Uint16",
            EnumWidth::U32 => "Uint32",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    Scalar,
    Nod,
    Enum { width: EnumWidth },
}

impl Ty {
    /// A capitalized type containing ':' is an enum, capitalized without is a nod,
    /// anything else is a scalar.
    pub fn classify(as_type: &str, opt_size: Option<&str>) -> Ty {
        if !as_type.chars().next().is_some_and(char::is_uppercase) {
            Ty::Scalar
        } else if as_type.contains(':') {
            Ty::Enum {
                width: EnumWidth::from_size(opt_size),
            }
        } else {
            Ty::Nod
        }
    }

    pub fn handle(self) -> &'static str {
        if matches!(self, Ty::Nod) {
            "@"
        } else {
            ""
        }
    }
}

impl Scalar {
    /// The Get*/Set* vocabulary for this member: rw_type override wins, then nod,
    /// then enum width, then the title-cased scalar type (`uint` -> `Uint32`).
    pub fn accessor_suffix(&self) -> String {
        if let Some(rw_type) = &self.rw_type {
            return rw_type.clone();
        }
        match self.ty {
            Ty::Nod => "Nod".to_string(),
            Ty::Enum { width } => width.suffix().to_string(),
            Ty::Scalar => title_case(&self.as_type),
        }
    }
}

/// Replaces Inflector's to_title_case for the corpus: known scalar types explicitly,
/// anything else gets its first letter uppercased (single-word types title-case the same).
fn title_case(as_type: &str) -> String {
    match as_type {
        "uint" => "Uint32".to_string(),
        "int" => "Int32".to_string(),
        "bool" => "Bool".to_string(),
        "float" => "Float".to_string(),
        "string" => "String".to_string(),
        "uint8" => "Uint8".to_string(),
        "uint16" => "Uint16".to_string(),
        "uint32" => "Uint32".to_string(),
        "uint64" => "Uint64".to_string(),
        "int8" => "Int8".to_string(),
        "int16" => "Int16".to_string(),
        "int32" => "Int32".to_string(),
        "int64" => "Int64".to_string(),
        "vec2" => "Vec2".to_string(),
        "vec3" => "Vec3".to_string(),
        "vec4" => "Vec4".to_string(),
        "iso4" => "Iso4".to_string(),
        "mat3" => "Mat3".to_string(),
        "nat2" => "Nat2".to_string(),
        "nat3" => "Nat3".to_string(),
        "quat" => "Quat".to_string(),
        _ => {
            let mut chars = as_type.chars();
            match chars.next() {
                Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                None => String::new(),
            }
        }
    }
}
