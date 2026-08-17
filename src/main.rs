use std::{fmt, io::{self, prelude::*, BufReader}, fs::File, path::PathBuf};
use clap::Parser;
use colorful::{Color, Colorful};
use inflector::{Inflector};

const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), " (", env!("GIT_HASH"), ")");

/// Generate AngelScript DevStructs from an .xtoml spec.
#[derive(Parser)]
#[command(name = "epp-codegen", version = VERSION, about)]
struct Cli {
    /// Path to the .xtoml file to compile.
    file_path: PathBuf,
}

static mut ERRORS: Vec<String> = vec![];
// Interim severity tracking: warnings share ERRORS but never affect the exit code.
static mut ERROR_COUNT: usize = 0;

fn add_error(msg: String) {
    unsafe {
        ERROR_COUNT += 1;
        ERRORS.push(msg);
    }
}

fn add_warning(msg: String) {
    unsafe {
        ERRORS.push(msg);
    }
}

#[derive(Clone, Debug, Default)]
struct Property {
    name: String,
    type_: String,
    offset: String,
    access: String,
    is_struct: bool,
    is_embedded: bool,
    comments: Vec<String>,
    opt_size: Option<String>,
    rw_type: Option<String>,
    inline_definition: Option<String>,
    line_num: usize,
}

#[derive(Clone, Debug)]
struct BufferProperty {
    name: String,
    class_name: String,
    offset: String,
    size: String,
    behind_ptr: String,
    comments: Vec<String>,
    line_num: usize,
}

impl BufferProperty {
    pub fn error(&self, msg: &str) -> String {
        format!("[{}]: {} (line {})", "ERROR".color(Color::Red3b), msg, self.line_num)
    }
}

#[derive(Clone, Debug)]
struct AngelScriptClass {
    name: String,
    size_check: String,
    native_class: Option<String>,
    properties: Vec<Property>,
    buffers: Vec<BufferProperty>,
    comments: Vec<String>,
    line_num: usize,
}

impl Property {
    pub fn capitalized_type(&self) -> String {
        if let Some(r) = &self.rw_type {
            return r.clone();
        }
        let to_append = match self.type_.as_str() {
            "uint" | "int" => "32",
            _ => ""
        };
        self.type_.to_title_case() + to_append
    }
    pub fn ret_handle(&self) -> String {
        if self.is_nod() {
            return String::from("@");
        }
        String::new()
    }
    pub fn is_enum(&self) -> bool {
        self.type_.contains(':') && self.is_type_capitalized()
    }
    pub fn is_nod(&self) -> bool {
        !self.is_enum() && self.is_type_capitalized()
    }
    pub fn is_type_capitalized(&self) -> bool {
        self.type_.chars().next().unwrap().is_uppercase()
    }
    pub fn enum_type_uint_size(&self) -> String {
        let sz_str = self.opt_size.clone().unwrap_or("4".to_string());
        match sz_str.as_str() {
            "1" => "uint8".to_string(),
            "2" => "uint16".to_string(),
            _ => "uint32".to_string(),
        }
    }
    pub fn error(&self, msg: &str) -> String {
        format!("[{}]: {} (line {})", "ERROR".color(Color::Red3b), msg, self.line_num)
    }
}

impl fmt::Display for Property {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // if we have an inline definition we just use that
        if let Some(inline_def) = &self.inline_definition {
            return write!(f, "\t{}\n", inline_def);
        }

        let comment = if self.comments.len() > 0 {
            "\t".to_string() + &self.comments.join("\n\t") + "\n"
        } else { String::new() };

        let mut result = String::new();
        let (get_set_type_name, cast_part) = match (self.is_nod(), self.is_enum()) {
            (true, false) => ("Nod".to_string(), format!("cast<{}>", self.type_)),
            (false, true) => (self.enum_type_uint_size().to_title_case(), self.type_.clone()),
            _ => (self.capitalized_type(), String::new()),
        };
        // if self.is_mw_id_value {
        //     get_set_type_name = "MwIdValue".to_string();
        // }

        if self.is_struct {
            if self.access.contains("S") {
                add_warning(format!("[{}]: Property {} is a struct ({}), Setter access not implemented.", "WARN".color(Color::Red3b), self.name.clone().color(Color::Orange3), self.type_.clone().color(Color::Turquoise2)));
            }
            result.push_str(&format!("{}\t{}{} get_{}() {{ auto _ptr = this.GetUint64({}); if (_ptr == 0) return null; return {}(_ptr); }}\n",
                                     comment,
                                     self.type_, self.ret_handle(), self.name, self.offset, self.type_));
        } else if self.is_embedded {
            if self.access.contains("S") {
                add_warning(format!("[{}]: Property {} is embedded ({}), Setter access not implemented.", "WARN".color(Color::Red3b), self.name.clone().color(Color::Orange3), self.type_.clone().color(Color::Turquoise2)));
            }
            result.push_str(&format!("{}\t{}{} get_{}() {{ return {}(this.Ptr + {}); }}\n",
                                     comment,
                                     self.type_, self.ret_handle(), self.name, self.type_, self.offset));
        } else {
            if self.access.contains("G") {
                result.push_str(&format!("{}\t{}{} get_{}() {{ return {}(this.Get{}({})); }}\n",
                comment,
                self.type_, self.ret_handle(), self.name, cast_part, get_set_type_name, self.offset));
            }
            if self.access.contains("S") {
                if self.type_ == "string" {
                    result.push_str(&format!("\tvoid set_{}(const string &in value) {{ this.Set{}({}, value); }}\n",
                    self.name, get_set_type_name, self.offset));
                } else {
                    result.push_str(&format!("\tvoid set_{}({}{} value) {{ this.Set{}({}, value); }}\n",
                    self.name, self.type_, self.ret_handle(), get_set_type_name, self.offset));
                }
            }
        }
        write!(f, "{}", result)
    }
}


impl AngelScriptClass {
    pub fn error(&self, msg: &str) -> String {
        format!("[{}]: {} (line {})", "ERROR".color(Color::Red3b), msg, self.line_num)
    }
}

impl fmt::Display for AngelScriptClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut extra_classes: Vec<String> = vec![];

        let mut properties_code = self.properties.iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join("");

        let native_class_code = if let Some(native_class) = &self.native_class {
            format!("\t{}({}@ nod) {{\n\t\tif (nod is null) throw(\"not a {}\");\n\t\tsuper(Dev_GetPointerForNod(nod), {});\n\t}}\n\t{}@ get_Nod() {{\n\t\treturn cast<{}>(Dev_GetNodFromPointer(ptr));\n\t}}\n",
                    self.name, native_class, native_class, self.size_check, native_class, native_class)
        } else {
            String::new()
        };

        for buffer in &self.buffers {
            let comment = if buffer.comments.len() > 0 {
                "\t".to_string() + &buffer.comments.join("\n\t") + "\n"
            } else { String::new() };

            properties_code = format!("{}{}\t{}@ get_{}() {{ return {}(this.GetBuffer({}, {}, {})); }}\n",
                    properties_code, comment, buffer.class_name, buffer.name,
                    buffer.class_name, buffer.offset, buffer.size, buffer.behind_ptr);

            let inner_type = buffer.class_name.strip_suffix('s').unwrap_or_else(|| {
                add_error(buffer.error("Buffer types must end in 's'"));
                buffer.class_name.as_str()
            });
            let get_fn_name = inner_type.split('_').last().unwrap();
            extra_classes.push(format!("class {} : RawBuffer {{\n\t{}(RawBuffer@ buf) {{\n\t\tsuper(buf.Ptr, buf.ElSize, buf.StructBehindPtr);\n\t}}\n\t{}@ Get{}(uint i) {{\n\t\treturn {}(this[i]);\n\t}}\n}}\n",
                    buffer.class_name, buffer.class_name, inner_type, get_fn_name, inner_type));
        }

        let extra_classes_str = extra_classes.join("\n\n");

        // println!("{:?}", self);

        let class_comments = if self.comments.len() > 0 {
            self.comments.join("\n") + "\n"
        } else { String::new() };

        write!(f, "{}class {} : RawBufferElem {{\n\t{}(RawBufferElem@ el) {{\n\t\tif (el.ElSize != {}) throw(\"invalid size for {}\");\n\t\tsuper(el.Ptr, el.ElSize);\n\t}}\n\t{}(uint64 ptr) {{\n\t\tsuper(ptr, {});\n\t}}\n{}\n{}}}\n\n{}",
               class_comments, self.name, self.name, self.size_check, self.name, self.name, self.size_check, native_class_code, properties_code, extra_classes_str)
    }
}

impl ExtraStrOps<'_> for String {
    fn trim_quotes(&'_ self) -> &'_ str {
        self.trim().trim_matches(|c| c == '"' || c == '\'')
    }
}
impl ExtraStrOps<'_> for str {
    fn trim_quotes(&'_ self) -> &'_ str {
        self.trim().trim_matches(|c| c == '"' || c == '\'')
    }
}
trait ExtraStrOps<'a> {
    fn trim_quotes(&'a self) -> &'a str;
}


fn main() -> io::Result<()> {
    let cli = Cli::parse();

    let file = match File::open(&cli.file_path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("error: cannot read {}: {}", cli.file_path.display(), e);
            std::process::exit(1);
        }
    };
    let reader = BufReader::new(file);

    let mut classes: Vec<AngelScriptClass> = vec![];
    let mut current_class: Option<AngelScriptClass> = None;
    let mut comments: Vec<String> = vec![];

    for (line_num, line) in reader.lines().enumerate() {
        let line = line?;
        let line_num = line_num + 1;
        if line.starts_with('[') {
            if let Some(class) = current_class.take() {
                classes.push(class);
                // Print the previous class
                // println!("{}", class);
            }

            let parts: Vec<&str> = line.trim_matches(|p| p == '[' || p == ']').trim_matches('"').split(':').map(|s| s.trim()).collect();
            current_class = Some(AngelScriptClass {
                name: parts[0].to_string(),
                size_check: parts[1].trim().to_string(),
                properties: Default::default(), // properties.clone(),
                native_class: None,
                buffers: Default::default(),
                comments: std::mem::take(&mut comments),
                line_num

            });
            // properties.clear();
        } else if !line.trim().is_empty() {
            if line.starts_with("#") {
                comments.push("// ".to_string() + line[1..].trim());
                continue;
            }
            if let Some(class) = current_class.as_mut() {
                let parts: Vec<&str> = line.split('=').map(|s| s.trim()).collect();
                let property_name = parts[0].trim_quotes();
                let property_details: Vec<&str> = parts.get(1).unwrap_or(&"").trim_quotes().split(',').map(|s| s.trim()).collect();

                if property_name == "NativeClass" {
                    class.native_class = Some(property_details[0].trim().to_string());
                } else if property_name.starts_with("Buffer: ") {
                    let buffer_name = property_name.split(':').nth(1).unwrap().trim_quotes();
                    let buffer_details: Vec<&str> = parts[1].trim_quotes().split(',').map(|s| s.trim()).collect();
                    class.buffers.push(BufferProperty {
                        name: buffer_name.to_string(),
                        class_name: buffer_details[0].to_string(),
                        offset: buffer_details[1].to_string(),
                        size: buffer_details[2].to_string(),
                        behind_ptr: buffer_details.get(3).map(|s| s.to_lowercase()).unwrap_or("false".to_string()),
                        comments,
                        line_num
                    });
                    comments = vec![];
                } else if property_name.starts_with("Struct: ") {
                    let struct_name = property_name.split(':').nth(1).unwrap().trim_quotes();
                    let struct_details: Vec<&str> = parts[1].trim_quotes().split(',').map(|s| s.trim()).collect();
                    class.properties.push(Property {
                        name: struct_name.to_string(),
                        type_: struct_details[0].to_string(),
                        offset: struct_details[1].to_string(),
                        access: struct_details[2].to_string(),
                        comments,
                        is_struct: true,
                        line_num,
                        ..Default::default()
                    });
                    comments = vec![];
                } else if property_name.starts_with("Embedded: ") {
                    let embedded_name = property_name.split(':').nth(1).unwrap().trim_quotes();
                    let embedded_details: Vec<&str> = parts[1].trim_quotes().split(',').map(|s| s.trim()).collect();
                    class.properties.push(Property {
                        name: embedded_name.to_string(),
                        type_: embedded_details[0].to_string(),
                        offset: embedded_details[1].to_string(),
                        access: embedded_details[2].to_string(),
                        comments,
                        is_embedded: true,
                        line_num,
                        ..Default::default()
                    });
                    comments = vec![];
                } else if let Some(defn) = line.strip_prefix("Inline: ") {
                    class.properties.push(Property {
                        inline_definition: Some(defn.to_string()),
                        comments,
                        line_num,
                        ..Default::default()
                    });
                    comments = vec![];
                } else {
                    // example: CGameCtnBlock::ECardinalDirections(4), 0x58, GS
                    let access = property_details.get(2).map(|s| s.trim()).unwrap_or("");
                    if !matches!(access, "G" | "S" | "GS") {
                        // invalid field line: skip silently, dropping any pending comments
                        comments = vec![];
                        continue;
                    }
                    let mut ty_parts = property_details[0].trim().split("(");
                    let type_ = ty_parts.nth(0).unwrap().to_string();
                    let opt_size = ty_parts.nth(0).and_then(|s| s.split(")").nth(0)).map(|s| s.to_string());
                    let rw_type = property_details.get(3).filter(|s| s.len() > 0).map(|s| s.to_string());
                    let property = Property {
                        name: property_name.to_string(),
                        type_,
                        offset: property_details[1].trim().to_string(),
                        access: access.to_string(),
                        opt_size,
                        rw_type,
                        comments,
                        line_num,
                        ..Default::default()
                    };
                    comments = vec![];
                    class.properties.push(property);
                }
            }
        }
    }

    // Process the last class
    if let Some(class) = current_class {
        // println!("{}", class);
        classes.push(class);
    }

    for cls in classes {
        println!("{}", cls);
    }

    eprintln!("Found {} errors:", unsafe { ERRORS.len() });
    for err in unsafe { ERRORS.iter() } {
        eprintln!("{}", err);
    }

    if unsafe { ERROR_COUNT } > 0 {
        std::process::exit(1);
    }

    Ok(())
}
