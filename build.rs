use std::env;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

use case::CaseExt;
use failure::{bail, err_msg, Error};
use indented::indented;

fn gen<P: AsRef<Path>>(pathes: &[P]) -> Result<(), Error> {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("protocol.rs");

    let mut f = File::create(dest_path)?;

    let mut events = vec![];

    writeln!(f, "use serde::{{Serialize, Deserialize}};")?;

    if cfg!(feature = "async") {
        writeln!(
            f,
            r#"
#[cfg(feature = "async")]
use futures::{{Future, Stream}};
"#
        )?;
    }

    for path in pathes {
        println!("cargo:rerun-if-changed={:?}", path.as_ref());

        let s = {
            let mut f = File::open(path)?;
            let mut s = String::new();
            f.read_to_string(&mut s)?;
            s
        };
        let (rest, pdl) = pdl::parse(&s).map_err(|_| err_msg("fail to parse PDL"))?;

        if !rest.is_empty() {
            bail!("unexpected: {}", rest)
        }

        writeln!(
            f,
            r#"{}#[doc(hidden)]
pub const {}_VERSION: &str = "{}.{}";"#,
            Comments(&pdl.description),
            path.as_ref()
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_uppercase(),
            pdl.version.major,
            pdl.version.minor
        )?;

        for domain in &pdl.domains {
            let experimental = if domain.experimental {
                "#[cfg(feature = \"experimental\")]\n"
            } else {
                ""
            };
            let deprecated = if domain.deprecated {
                "#[deprecated]\n"
            } else {
                ""
            };

            writeln!(
                f,
                r#"
{}{}{}#[cfg(any(feature = "all", feature = "{}"))]
pub trait {}{} {{
    type Error;
{}}}"#,
                Comments(&domain.description),
                experimental,
                deprecated,
                domain.name.to_snake(),
                domain.name,
                if domain.dependencies.is_empty() {
                    "".to_owned()
                } else {
                    let dependencies = domain
                        .dependencies
                        .iter()
                        .map(|name| format!("crate::{}", name))
                        .collect::<Vec<_>>();

                    format!(": {}", dependencies.join(" + "))
                },
                indented(Trait(domain))
            )?;

            if cfg!(feature = "async") {
                writeln!(
                    f,
                    r#"
{}{}{}#[cfg(all(feature = "async", any(feature = "all", feature = "{}")))]
pub trait Async{}{} {{
    type Error;
{}}}"#,
                    Comments(&domain.description),
                    experimental,
                    deprecated,
                    domain.name.to_snake(),
                    domain.name,
                    if domain.dependencies.is_empty() {
                        "".to_owned()
                    } else {
                        let dependencies = std::iter::once(domain.name.to_owned())
                            .chain(
                                domain
                                    .dependencies
                                    .iter()
                                    .map(|name| format!("crate::Async{}", name)),
                            )
                            .collect::<Vec<_>>();

                        format!(": {}", dependencies.join(" + "))
                    },
                    indented(AsyncTrait(domain))
                )?;
            }

            writeln!(
                f,
                r#"
{}{}{}#[cfg(any(feature = "all", feature = "{3}"))]
#[allow(deprecated)]
pub mod {} {{
{}}}"#,
                Comments(&domain.description),
                experimental,
                deprecated,
                domain.name.to_snake(),
                indented(Mod(domain))
            )?;

            for evt in &domain.events {
                events.push(format!(
                    r#"{}{}{}#[cfg(any(feature = "all", feature = "{}"))]
#[serde(rename = "{}.{}")]
{}{}({3}::{7}Event),"#,
                    Comments(&evt.description),
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
                    domain.name.to_snake(),
                    domain.name,
                    evt.name,
                    domain.name.to_capitalized(),
                    evt.name.to_capitalized()
                ));
            }
        }
    }

    writeln!(
        f,
        r#"
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "method")]
#[allow(clippy::large_enum_variant)]
pub enum Event {{
{}
}}
"#,
        indented(events.join("\n\n"))
    )?;

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

struct Trait<'a>(&'a pdl::Domain<'a>);

impl<'a> fmt::Display for Trait<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let domain = self.0;

        if !domain.events.is_empty() {
            writeln!(
                f,
                r#"type Events: Iterator<Item = {}::Event>;

fn events(&self) -> <Self as {}>::Events;"#,
                domain.name.to_snake(),
                domain.name
            )?;
        }

        for cmd in &domain.commands {
            writeln!(
                f,
                "\n{}{}{}fn {}(&self, req: {}::{}Request) -> Result<{4}::{5}, <Self as {}>::Error>;",
                Comments(&cmd.description),
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
                cmd.name.to_snake(),
                domain.name.to_snake(),
                cmd.name.to_capitalized(),
                        domain.name,

            )?;
        }

        Ok(())
    }
}

struct AsyncTrait<'a>(&'a pdl::Domain<'a>);

impl<'a> fmt::Display for AsyncTrait<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let domain = self.0;

        for cmd in &domain.commands {
            writeln!(
                f,
                "{}type {}: Future<Item = {}::{1}Response, Error = <Self as Async{}>::Error>;",
                if cmd.experimental {
                    "#[cfg(feature = \"experimental\")]\n"
                } else {
                    ""
                },
                cmd.name.to_capitalized(),
                domain.name.to_snake(),
                domain.name,
            )?;
        }

        if !domain.events.is_empty() {
            writeln!(
                f,
                r#"type Events: Stream<Item = {}::Event, Error = <Self as Async{}>::Error>;

fn events(&self) -> <Self as Async{1}>::Events;"#,
                domain.name.to_snake(),
                domain.name
            )?;
        }

        for cmd in &domain.commands {
            writeln!(
                f,
                "\n{}{}{}fn {}(&self, req: {}::{}Request) -> <Self as Async{}>::{5};",
                Comments(&cmd.description),
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
                cmd.name.to_snake(),
                domain.name.to_snake(),
                cmd.name.to_capitalized(),
                domain.name,
            )?;
        }

        Ok(())
    }
}

struct Mod<'a>(&'a pdl::Domain<'a>);

impl<'a> fmt::Display for Mod<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let domain = self.0;
        let mut events = vec![];

        writeln!(
            f,
            r#"use serde::{{Serialize, Deserialize}};

use crate::*;"#
        )?;

        for ty in &domain.types {
            write!(f, "\n{}", Comments(&ty.description))?;

            match ty.item {
                Some(pdl::Item::Enum(ref variants)) => {
                    write!(
                        f,
                        "{}{}",
                        if ty.deprecated { "#[deprecated]\n" } else { "" },
                        Enum(ty.id, variants)
                    )?;
                }
                Some(pdl::Item::Properties(ref props)) => {
                    write!(
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
                        "pub type {} = {};",
                        ty.id,
                        Type(&ty.extends, Some(ty.id))
                    )?;
                }
            }
        }

        for cmd in &domain.commands {
            let request = format!("{}Request", cmd.name.to_capitalized());
            let response = format!("{}Response", cmd.name.to_capitalized());
            let return_object = format!("{}ReturnObject", cmd.name.to_capitalized());
            let experimental = if cmd.experimental {
                "#[cfg(feature = \"experimental\")]\n"
            } else {
                ""
            };
            let deprecated = if cmd.deprecated {
                "#[deprecated]\n"
            } else {
                ""
            };

            writeln!(
                f,
                r#"
/// Method parameters of the `{}::{}` command.
{}{}pub type {} = {};"#,
                domain.name,
                cmd.name,
                experimental,
                deprecated,
                cmd.name.to_capitalized(),
                request
            )?;
            writeln!(
                f,
                r#"
/// Return object of the `{}::{}` command.
{}{}pub type {} = {};"#,
                domain.name, cmd.name, experimental, deprecated, return_object, response
            )?;
            write!(
                f,
                r#"
/// Request object of the `{}::{}` command.
{}{}{}"#,
                domain.name,
                cmd.name,
                experimental,
                deprecated,
                Struct(&request, &cmd.parameters)
            )?;
            write!(
                f,
                r#"
/// Response object of the `{}::{}` command.
{}{}{}"#,
                domain.name,
                cmd.name,
                experimental,
                deprecated,
                Struct(&response, &cmd.returns)
            )?;
            writeln!(
                f,
                r#"
{}impl Method for {} {{
    const NAME: &'static str = "{}.{}";

    type ReturnObject = {};
}}"#,
                experimental,
                cmd.name.to_capitalized(),
                domain.name,
                cmd.name,
                return_object,
            )?;
        }

        for evt in &domain.events {
            let experimental = if evt.experimental {
                "#[cfg(feature = \"experimental\")]\n"
            } else {
                ""
            };
            let deprecated = if evt.deprecated {
                "#[deprecated]\n"
            } else {
                ""
            };

            writeln!(
                f,
                "{}{}{}{}",
                Comments(&evt.description),
                experimental,
                deprecated,
                Struct(
                    &format!("{}Event", evt.name.to_capitalized()),
                    &evt.parameters
                )
            )?;

            events.push(format!(
                r#"{}{}{}#[serde(rename = "{}.{}")]
{}({5}Event),
"#,
                Comments(&evt.description),
                experimental,
                deprecated,
                domain.name,
                evt.name,
                evt.name.to_capitalized(),
            ));
        }

        if !events.is_empty() {
            writeln!(
                f,
                r#"#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "method")]
#[allow(clippy::large_enum_variant)]
pub enum Event {{
{}
}}"#,
                indented(events.join("\n"))
            )?;
        }

        Ok(())
    }
}

struct Type<'a>(&'a pdl::Type<'a>, Option<&'a str>);

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            pdl::Type::Integer => write!(f, "Integer"),
            pdl::Type::Number => write!(f, "Number"),
            pdl::Type::Boolean => write!(f, "Boolean"),
            pdl::Type::String => write!(f, "String"),
            pdl::Type::Object => write!(f, "Object"),
            pdl::Type::Any => write!(f, "Any"),
            pdl::Type::Binary => write!(f, "Binary"),
            pdl::Type::Enum(_) => unreachable! {},
            pdl::Type::ArrayOf(ty) => write!(f, "Vec<{}>", Type(&ty, None)),
            pdl::Type::Ref(id) => {
                let parts = id.split('.').collect::<Vec<_>>();
                let (last, parts) = parts.split_last().unwrap();
                let ref_ty = parts
                    .iter()
                    .map(|s| s.to_snake())
                    .chain(std::iter::once(last.to_string()))
                    .collect::<Vec<_>>()
                    .join("::");

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
#[serde(rename_all = "kebab-case")]
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
            self.0.name.replace('-', "_").to_camel()
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
        let param = self.0;
        let mangled_name = mangle(param.name);

        write!(
            f,
            "{}{}{}{}pub {}: ",
            Comments(&param.description),
            if param.experimental {
                "#[cfg(feature = \"experimental\")]\n"
            } else {
                ""
            },
            if param.deprecated {
                "#[deprecated]\n"
            } else {
                ""
            },
            if mangled_name.to_camel_lowercase() != param.name {
                format!("#[serde(rename = \"{}\")]\n", param.name)
            } else {
                "".to_owned()
            },
            mangled_name
        )?;

        let ty = if let pdl::Type::Enum(_) = param.ty {
            format!("{}{}", self.1, param.name.to_capitalized())
        } else {
            Type(&param.ty, Some(self.1)).to_string()
        };

        if param.optional {
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
    gen(&[
        "devtools-protocol/pdl/browser_protocol.pdl",
        "devtools-protocol/pdl/js_protocol.pdl",
    ])?;

    Ok(())
}
