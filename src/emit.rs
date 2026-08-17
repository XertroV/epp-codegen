//! FileAst -> AngelScript. All formatting lives here: tabs, parens, blank-line rules,
//! deferred buffer getters, and wrapper classes. Templates read as AngelScript.

use crate::ast::{ClassAst, FileAst, Member, Scalar, Ty};
use crate::error::Diagnostic;

pub fn emit(ast: &FileAst) -> (String, Vec<Diagnostic>) {
    let mut out = String::new();
    for class in &ast.classes {
        emit_class(class, &mut out);
        out.push('\n');
    }
    (out, Vec::new())
}

fn emit_class(class: &ClassAst, out: &mut String) {
    let mut members_code = String::new();
    for member in &class.members {
        emit_member(member, &mut members_code);
    }

    let native_class_code = match &class.native_class {
        Some(native) => format!(
            "\t{}({}@ nod) {{\n\t\tif (nod is null) throw(\"not a {}\");\n\t\tsuper(Dev_GetPointerForNod(nod), {});\n\t}}\n\t{}@ get_Nod() {{\n\t\treturn cast<{}>(Dev_GetNodFromPointer(ptr));\n\t}}\n",
            class.name, native, native, class.size_check, native, native
        ),
        None => String::new(),
    };

    // Buffer getters are deferred to the end of the class body; wrapper classes follow it.
    let mut extra_classes: Vec<String> = Vec::new();
    for buffer in &class.buffers {
        members_code.push_str(&comment_block(&buffer.comments));
        members_code.push_str(&format!(
            "\t{}@ get_{}() {{ return {}(this.GetBuffer({}, {}, {})); }}\n",
            buffer.wrapper,
            buffer.name,
            buffer.wrapper,
            buffer.offset,
            buffer.size,
            buffer.behind_ptr
        ));
        extra_classes.push(format!(
            "class {} : RawBuffer {{\n\t{}(RawBuffer@ buf) {{\n\t\tsuper(buf.Ptr, buf.ElSize, buf.StructBehindPtr);\n\t}}\n\t{}@ {}(uint i) {{\n\t\treturn {}(this[i]);\n\t}}\n}}\n",
            buffer.wrapper, buffer.wrapper, buffer.inner, buffer.get_fn, buffer.inner
        ));
    }
    let extra_classes_str = extra_classes.join("\n\n");

    let class_comments = if class.comments.is_empty() {
        String::new()
    } else {
        class.comments.join("\n") + "\n"
    };

    out.push_str(&format!(
        "{}class {} : RawBufferElem {{\n\t{}(RawBufferElem@ el) {{\n\t\tif (el.ElSize != {}) throw(\"invalid size for {}\");\n\t\tsuper(el.Ptr, el.ElSize);\n\t}}\n\t{}(uint64 ptr) {{\n\t\tsuper(ptr, {});\n\t}}\n{}\n{}}}\n\n{}",
        class_comments,
        class.name,
        class.name,
        class.size_check,
        class.name,
        class.name,
        class.size_check,
        native_class_code,
        members_code,
        extra_classes_str
    ));
}

fn emit_member(member: &Member, out: &mut String) {
    match member {
        Member::Inline(inline) => {
            out.push_str(&format!("\t{}\n", inline.code));
        }
        Member::StructPtr(prop) => {
            out.push_str(&comment_block(&prop.comments));
            out.push_str(&format!(
                "\t{}{} get_{}() {{ auto _ptr = this.GetUint64({}); if (_ptr == 0) return null; return {}(_ptr); }}\n",
                prop.as_type,
                prop.ty.handle(),
                prop.name,
                prop.offset,
                prop.as_type
            ));
        }
        Member::Embedded(prop) => {
            out.push_str(&comment_block(&prop.comments));
            out.push_str(&format!(
                "\t{}{} get_{}() {{ return {}(this.Ptr + {}); }}\n",
                prop.as_type,
                prop.ty.handle(),
                prop.name,
                prop.as_type,
                prop.offset
            ));
        }
        Member::Scalar(scalar) => emit_scalar(scalar, out),
    }
}

/// `cast<T>` is a one-liner so the getter template reads as AngelScript.
fn cast_part(scalar: &Scalar) -> String {
    match scalar.ty {
        Ty::Nod => format!("cast<{}>", scalar.as_type),
        Ty::Enum { .. } => scalar.as_type.clone(),
        Ty::Scalar => String::new(),
    }
}

fn emit_scalar(scalar: &Scalar, out: &mut String) {
    out.push_str(&comment_block(&scalar.comments));
    let suffix = scalar.accessor_suffix();
    if scalar.access.readable() {
        out.push_str(&format!(
            "\t{}{} get_{}() {{ return {}(this.Get{}({})); }}\n",
            scalar.as_type,
            scalar.ty.handle(),
            scalar.name,
            cast_part(scalar),
            suffix,
            scalar.offset
        ));
    }
    if scalar.access.writable() {
        if scalar.as_type == "string" {
            out.push_str(&format!(
                "\tvoid set_{}(const string &in value) {{ this.Set{}({}, value); }}\n",
                scalar.name, suffix, scalar.offset
            ));
        } else {
            out.push_str(&format!(
                "\tvoid set_{}({}{} value) {{ this.Set{}({}, value); }}\n",
                scalar.name,
                scalar.as_type,
                scalar.ty.handle(),
                suffix,
                scalar.offset
            ));
        }
    }
}

fn comment_block(comments: &[String]) -> String {
    if comments.is_empty() {
        String::new()
    } else {
        format!("\t{}\n", comments.join("\n\t"))
    }
}
