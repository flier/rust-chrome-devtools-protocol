use std::env;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

use case::CaseExt;
use failure::{bail, err_msg, Error};
use indented::indented;

fn gen<P: AsRef<Path>>(path: P) -> Result<(), Error> {
    println!("cargo:rerun-if-changed={:?}", path.as_ref());

    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir)
        .join(path.as_ref().file_name().unwrap())
        .with_extension("rs");

    let mut f = File::open(path)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;

    let (rest, pdl) = pdl::parse(&s).map_err(|_| err_msg("fail to parse PDL"))?;

    if !rest.is_empty() {
        bail!("unexpected: {}", rest)
    }

    let mut f = File::create(dest_path)?;

    writeln!(
        f,
        "{}pub const PROTOCOL_VERSION: &str = \"{}.{}\";\n",
        Comments(&pdl.description),
        pdl.version.major,
        pdl.version.minor
    )?;

    for domain in pdl.domains {
        writeln!(
            f,
            r#"{}{}{}pub mod {} {{
{}}}
"#,
            Comments(&domain.description),
            if domain.experimental {
                "#[cfg(feature = \"experimental\")]\n"
            } else {
                ""
            },
            if domain.deprecated {
                "#[deprecated]\n"
            } else {
                ""
            },
            domain.name,
            indented(Mod(&domain))
        )?;
    }

    Ok(())
}

struct Comments<'a>(&'a pdl::Description<'a>);

impl<'a> fmt::Display for Comments<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for comment in self.0.iter() {
            writeln!(f, "/// {}", comment)?;
        }

        Ok(())
    }
}

struct Mod<'a>(&'a pdl::Domain<'a>);

impl<'a> fmt::Display for Mod<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            r#"#![allow(deprecated, non_snake_case, unused_imports)]

use serde::{{Serialize, Deserialize}};

use crate::*;
"#
        )?;

        for ty in &self.0.types {
            write!(f, "{}", Comments(&ty.description))?;

            match ty.item {
                Some(pdl::Item::Enum(ref variants)) => {
                    writeln!(
                        f,
                        "{}{}",
                        if ty.deprecated { "#[deprecated]\n" } else { "" },
                        Enum(ty.id, variants)
                    )?;
                }
                Some(pdl::Item::Properties(ref props)) => {
                    writeln!(
                        f,
                        "{}{}{}",
                        if ty.experimental {
                            "#[cfg(feature = \"experimental\")]\n"
                        } else {
                            ""
                        },
                        if ty.deprecated { "#[deprecated]\n" } else { "" },
                        Struct(ty.id, props)
                    )?;
                }
                None => {
                    writeln!(
                        f,
                        "pub type {} = {};\n",
                        ty.id,
                        Type(&ty.extends, Some(ty.id))
                    )?;
                }
            }
        }

        for cmd in &self.0.commands {
            writeln!(
                f,
                "/// The `{}::{}` command.\n{}{}pub type {} = {4}Request;\n",
                self.0.name,
                cmd.name,
                if cmd.experimental {
                    "#[cfg(feature = \"experimental\")]\n"
                } else {
                    ""
                },
                if cmd.deprecated {
                    "#[deprecated]\n"
                } else {
                    ""
                },
                cmd.name.to_capitalized(),
            )?;
            writeln!(
                f,
                "/// Request parameters to the `{}::{}` command.\n{}{}{}",
                self.0.name,
                cmd.name,
                if cmd.experimental {
                    "#[cfg(feature = \"experimental\")]\n"
                } else {
                    ""
                },
                if cmd.deprecated {
                    "#[deprecated]\n"
                } else {
                    ""
                },
                Struct(
                    &format!("{}Request", cmd.name.to_capitalized()),
                    &cmd.parameters
                )
            )?;
            writeln!(
                f,
                "/// Response returns from the `{}::{}` command.\n{}{}{}",
                self.0.name,
                cmd.name,
                if cmd.experimental {
                    "#[cfg(feature = \"experimental\")]\n"
                } else {
                    ""
                },
                if cmd.deprecated {
                    "#[deprecated]\n"
                } else {
                    ""
                },
                Struct(
                    &format!("{}Response", cmd.name.to_capitalized()),
                    &cmd.returns
                )
            )?;
        }

        for evt in &self.0.events {
            writeln!(
                f,
                "/// Event parameters for the `{}::{}` event.\n{}{}{}",
                self.0.name,
                evt.name,
                if evt.experimental {
                    "#[cfg(feature = \"experimental\")]\n"
                } else {
                    ""
                },
                if evt.deprecated {
                    "#[deprecated]\n"
                } else {
                    ""
                },
                Struct(
                    &format!("{}Event", evt.name.to_capitalized()),
                    &evt.parameters
                )
            )?;
        }

        Ok(())
    }
}

struct Type<'a>(&'a pdl::Type<'a>, Option<&'a str>);

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            pdl::Type::Integer => write!(f, "isize"),
            pdl::Type::Number => write!(f, "f64"),
            pdl::Type::Boolean => write!(f, "bool"),
            pdl::Type::String => write!(f, "String"),
            pdl::Type::Object => write!(f, "Object"),
            pdl::Type::Any => write!(f, "Any"),
            pdl::Type::Binary => write!(f, "Vec<u8>"),
            pdl::Type::Enum(_) => unreachable! {},
            pdl::Type::ArrayOf(ty) => write!(f, "Vec<{}>", Type(&ty, None)),
            pdl::Type::Ref(id) => {
                let ref_ty = id.replace('.', "::");

                if self.1 == Some(id) {
                    write!(f, "Box<{}>", ref_ty)
                } else {
                    write!(f, "{}", ref_ty)
                }
            }
        }
    }
}

struct Enum<'a>(&'a str, &'a [pdl::Variant<'a>]);

impl<'a> fmt::Display for Enum<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            r#"#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum {} {{"#,
            self.0,
        )?;
        for var in self.1 {
            writeln!(f, "{},", indented(Variant(&var)))?;
        }
        writeln!(f, "}}")
    }
}

struct Variant<'a>(&'a pdl::Variant<'a>);

impl<'a> fmt::Display for Variant<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}",
            Comments(&self.0.description),
            self.0.name.replace('-', "").to_camel()
        )
    }
}

struct Struct<'a>(&'a str, &'a [pdl::Param<'a>]);

impl<'a> fmt::Display for Struct<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            r#"#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct {} {{"#,
            self.0
        )?;

        for prop in self.1 {
            writeln!(f, "{},", indented(Field(&prop, self.0)))?;
        }

        writeln!(f, "}}")?;

        for prop in self.1 {
            if let pdl::Type::Enum(ref variants) = prop.ty {
                write!(
                    f,
                    "\n/// Allow values for the `{}::{}` field.\n{}{}",
                    self.0,
                    prop.name,
                    if prop.experimental {
                        "#[cfg(feature = \"experimental\")]\n"
                    } else {
                        ""
                    },
                    Enum(
                        &format!("{}{}", self.0, prop.name.to_capitalized()),
                        variants
                    )
                )?;
            }
        }

        Ok(())
    }
}

struct Field<'a>(&'a pdl::Param<'a>, &'a str);

impl<'a> fmt::Display for Field<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}{}pub {}: ",
            Comments(&self.0.description),
            if self.0.experimental {
                "#[cfg(feature = \"experimental\")]\n"
            } else {
                ""
            },
            if self.0.deprecated {
                "#[deprecated]\n"
            } else {
                ""
            },
            mangle(self.0.name)
        )?;

        let ty = if let pdl::Type::Enum(_) = self.0.ty {
            format!("{}{}", self.1, self.0.name.to_capitalized())
        } else {
            Type(&self.0.ty, Some(self.1)).to_string()
        };

        if self.0.optional {
            write!(f, "Option<{}>", ty)
        } else {
            write!(f, "{}", ty)
        }
    }
}

const KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "dyn", "abstract", "become", "box", "do", "final", "macro", "override", "priv",
    "typeof", "unsized", "virtual", "yield", "async", "await", "try", "union",
];

fn mangle(name: &str) -> String {
    let name = name.replace('-', "").to_snake();

    if KEYWORDS.contains(&name.as_str()) {
        format!("r#{}", name)
    } else {
        name
    }
}

fn main() -> Result<(), Error> {
    gen("devtools-protocol/pdl/browser_protocol.pdl")?;
    gen("devtools-protocol/pdl/js_protocol.pdl")?;

    Ok(())
}
