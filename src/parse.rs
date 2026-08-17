//! xtoml -> FileAst: the line state machine. Class comments flush on `[`, field
//! comments attach to the next directive, dangling comments are dropped, an invalid
//! access column skips the line with a Warn.

use crate::ast::{Access, Buffer, ClassAst, FileAst, InlineProp, Member, RefProp, Scalar, Ty};
use crate::error::Diagnostic;

pub fn parse(input: &str) -> (FileAst, Vec<Diagnostic>) {
    let mut classes: Vec<ClassAst> = Vec::new();
    let mut current_class: Option<ClassAst> = None;
    let mut comments: Vec<String> = Vec::new();
    let mut diagnostics: Vec<Diagnostic> = Vec::new();

    for (line_num, line) in input.lines().enumerate() {
        let line_num = line_num + 1;
        if line.starts_with('[') {
            if let Some(class) = current_class.take() {
                classes.push(class);
            }
            let parts: Vec<&str> = line
                .trim_matches(|p| p == '[' || p == ']')
                .trim_matches('"')
                .split(':')
                .map(|s| s.trim())
                .collect();
            if parts.len() < 2 {
                diagnostics.push(Diagnostic::error(
                    line_num,
                    format!("malformed class header: {}", line),
                ));
                continue;
            }
            current_class = Some(ClassAst {
                name: parts[0].to_string(),
                size_check: parts[1].to_string(),
                native_class: None,
                members: Vec::new(),
                buffers: Vec::new(),
                comments: std::mem::take(&mut comments),
            });
        } else if !line.trim().is_empty() {
            if let Some(rest) = line.strip_prefix('#') {
                comments.push("// ".to_string() + rest.trim());
                continue;
            }
            if let Some(class) = current_class.as_mut() {
                let parts: Vec<&str> = line.split('=').map(|s| s.trim()).collect();
                let property_name = parts[0].trim_quotes();
                let property_details: Vec<&str> = parts
                    .get(1)
                    .unwrap_or(&"")
                    .trim_quotes()
                    .split(',')
                    .map(|s| s.trim())
                    .collect();

                if property_name == "NativeClass" {
                    class.native_class = Some(property_details[0].to_string());
                } else if property_name.starts_with("Buffer: ") {
                    let name = property_name.split(':').nth(1).unwrap().trim_quotes();
                    if property_details.len() < 3 {
                        diagnostics.push(Diagnostic::error(
                            line_num,
                            format!("malformed Buffer directive for {}", name),
                        ));
                        comments.clear();
                        continue;
                    }
                    let behind_ptr = property_details
                        .get(3)
                        .map(|s| s.to_lowercase())
                        .unwrap_or_else(|| "false".to_string());
                    let (buffer, diagnostic) = Buffer::new(
                        name.to_string(),
                        property_details[0].to_string(),
                        property_details[1].to_string(),
                        property_details[2].to_string(),
                        behind_ptr,
                        std::mem::take(&mut comments),
                        line_num,
                    );
                    if let Some(diagnostic) = diagnostic {
                        diagnostics.push(diagnostic);
                    }
                    class.buffers.push(buffer);
                } else if property_name.starts_with("Struct: ") || property_name.starts_with("Embedded: ")
                {
                    let is_struct = property_name.starts_with("Struct: ");
                    let name = property_name.split(':').nth(1).unwrap().trim_quotes();
                    if property_details.len() < 3 {
                        let kind = if is_struct { "Struct" } else { "Embedded" };
                        diagnostics.push(Diagnostic::error(
                            line_num,
                            format!("malformed {} directive for {}", kind, name),
                        ));
                        comments.clear();
                        continue;
                    }
                    let as_type = property_details[0].to_string();
                    if property_details[2].contains('S') {
                        let kind = if is_struct { "a struct" } else { "embedded" };
                        diagnostics.push(Diagnostic::warn(
                            line_num,
                            format!(
                                "Property {} is {} ({}), Setter access not implemented.",
                                name, kind, as_type
                            ),
                        ));
                    }
                    let prop = RefProp {
                        name: name.to_string(),
                        ty: Ty::classify(&as_type, None),
                        as_type,
                        offset: property_details[1].to_string(),
                        comments: std::mem::take(&mut comments),
                    };
                    if is_struct {
                        class.members.push(Member::StructPtr(prop));
                    } else {
                        class.members.push(Member::Embedded(prop));
                    }
                } else if let Some(defn) = line.strip_prefix("Inline: ") {
                    class.members.push(Member::Inline(InlineProp {
                        code: defn.to_string(),
                    }));
                    comments.clear();
                } else {
                    // example: CGameCtnBlock::ECardinalDirections(4), 0x58, GS
                    let access_str = property_details.get(2).copied().unwrap_or("");
                    let access = match Access::parse(access_str) {
                        Some(access) => access,
                        None => {
                            diagnostics.push(Diagnostic::warn(
                                line_num,
                                format!(
                                    "invalid access \"{}\" for property \"{}\"; skipping line",
                                    access_str, property_name
                                ),
                            ));
                            comments.clear();
                            continue;
                        }
                    };
                    let mut ty_parts = property_details[0].split('(');
                    let as_type = ty_parts.next().unwrap_or("").to_string();
                    let opt_size = ty_parts
                        .next()
                        .and_then(|s| s.split(')').next())
                        .map(|s| s.to_string());
                    let rw_type = property_details
                        .get(3)
                        .filter(|s| !s.is_empty())
                        .map(|s| s.to_string());
                    let ty = Ty::classify(&as_type, opt_size.as_deref());
                    class.members.push(Member::Scalar(Scalar {
                        name: property_name.to_string(),
                        ty,
                        as_type,
                        offset: property_details[1].to_string(),
                        access,
                        rw_type,
                        comments: std::mem::take(&mut comments),
                    }));
                }
            }
        }
    }

    if let Some(class) = current_class {
        classes.push(class);
    }

    (FileAst { classes }, diagnostics)
}

trait ExtraStrOps {
    fn trim_quotes(&self) -> &str;
}

impl ExtraStrOps for str {
    fn trim_quotes(&self) -> &str {
        self.trim().trim_matches(|c| c == '"' || c == '\'')
    }
}
