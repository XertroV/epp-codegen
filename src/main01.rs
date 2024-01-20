use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    let file = File::open("input_spec.toml")?;
    let reader = BufReader::new(file);

    let mut class_name = String::new();
    let mut size_check = String::new();

    for line in reader.lines() {
        let line = line?;
        if line.starts_with('[') {
            // Process class header
            if !class_name.is_empty() {
                // Generate code for the previous class
                generate_class(&class_name, &size_check);
            }
            let parts: Vec<&str> = line.trim_matches(|p| p == '[' || p == ']').split(':').collect();
            class_name = parts[0].to_string();
            size_check = parts[1].trim().to_string();
        } else if !line.trim().is_empty() {
            // Process properties
            let parts: Vec<&str> = line.split('=').collect();
            let property_name = parts[0].trim();
            let property_details: Vec<&str> = parts[1].trim_matches(|p| p == '"' || p == '\'').split(',').collect();

            let property_type = property_details[0].trim();
            let offset = property_details[1].trim();
            let access = property_details[2].trim();

            generate_property(&property_name, property_type, offset, access);
        }
    }

    // Generate code for the last class
    generate_class(&class_name, &size_check);

    Ok(())
}

fn generate_class(class_name: &str, size_check: &str) {
    println!("class {} : RawBufferElem {{", class_name);
    println!("\t{}(RawBufferElem@ el) {{", class_name);
    println!("\t\tif (el.ElSize != {}) throw(\"invalid size for {}\");", size_check, class_name);
    println!("\t\tsuper(el.Ptr, el.ElSize);");
    println!("\t}}\n\t// Properties");
}

fn generate_property(name: &str, type_: &str, offset: &str, access: &str) {
    if access.contains("G") {
        println!("\t{} get_{}() {{ return this.Get{}({}); }}", type_, name, type_, offset);
    }
    if access.contains("S") {
        println!("\tvoid set_{}({} value) {{ this.Set{}({}, value); }}", name, type_, type_, offset);
    }
}
