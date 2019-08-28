use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fs::File;
use std::io::{self, prelude::*};
use std::path::Path;

use case::CaseExt;
use failure::{bail, err_msg, Error};
use indented::indented;

const MAX_INLINE_PARAMS: usize = 7;
const MAX_INLINE_RETURNS: usize = 3;

const EVENTS: &str = r#"pub trait Events {
    type Events: Iterator<Item = Event>;

    fn events(&self) -> Self::Events;
}"#;

const ASYNC_EVENTS: &str = r#"
pub trait AsyncEvents {
    type Error;
    type Events: futures::Stream<Item = Event, Error = Self::Error>;

    fn events(&self) -> Self::Events;
}
"#;

fn gen<P: AsRef<Path>>(pathes: &[P]) -> Result<(), Error> {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("protocol.rs");

    let mut f = File::create(dest_path)?;

    let mut events = vec![];

    writeln!(
        f,
        r#"{}{}
use crate::CallSite;
"#,
        if cfg!(feature = "server") {
            "use serde::Serialize;\n"
        } else {
            ""
        },
        if cfg!(feature = "client") {
            "use serde::Deserialize;\n"
        } else {
            ""
        }
    )?;

    if cfg!(feature = "async") {
        writeln!(
            f,
            r#"use futures::Future;

use crate::AsyncCallSite;
"#
        )?;
    }

    let files = pathes
        .iter()
        .map(|path| {
            let path = path.as_ref();

            println!("cargo:rerun-if-changed={:?}", path);

            let mut f = File::open(path)?;
            let mut s = String::new();
            f.read_to_string(&mut s)?;

            Ok((path.to_owned(), s))
        })
        .collect::<Result<Vec<_>, io::Error>>()?;

    let pdls = files
        .iter()
        .map(|(path, s)| {
            let (rest, pdl) = pdl::parse(&s).map_err(|_| err_msg("fail to parse PDL"))?;

            if !rest.is_empty() {
                bail!("unexpected: {}", rest)
            }

            Ok((path.as_path(), pdl))
        })
        .collect::<Result<Vec<_>, Error>>()?;

    let domains = pdls
        .iter()
        .flat_map(|(_, pdl)| pdl.domains.iter().map(|domain| (domain.name, domain)))
        .collect::<HashMap<_, _>>();

    let types = domains
        .iter()
        .flat_map(|(name, domain)| {
            domain
                .types
                .iter()
                .map(move |ty| ((*name, ty.id), (*domain, ty)))
        })
        .collect::<HashMap<_, _>>();

    fn has_default(
        types: &HashMap<(&str, &str), (&pdl::Domain, &pdl::TypeDef)>,
        domain: &pdl::Domain,
        struct_name: Option<&str>,
        ty: &pdl::Type,
    ) -> bool {
        match ty {
            pdl::Type::Integer
            | pdl::Type::Number
            | pdl::Type::Boolean
            | pdl::Type::String
            | pdl::Type::Object
            | pdl::Type::Any
            | pdl::Type::Binary
            | pdl::Type::ArrayOf(_) => true,
            pdl::Type::Enum(_) => false,
            pdl::Type::Ref(id) if struct_name.map_or(false, |name| name == *id) => true,
            pdl::Type::Ref(id) => {
                let (domain_name, type_id) = if id.contains('.') {
                    let mut iter = id.splitn(2, '.');

                    (iter.next().unwrap(), iter.next().unwrap())
                } else {
                    (domain.name, *id)
                };

                let (domain, typedef) = types.get(&(domain_name, type_id)).unwrap();

                match typedef.item {
                    Some(pdl::Item::Enum(_)) => false,
                    Some(pdl::Item::Properties(ref params)) => params.iter().all(|param| {
                        param.optional || has_default(types, domain, struct_name, &param.ty)
                    }),
                    None => has_default(types, domain, struct_name, &typedef.extends),
                }
            }
        }
    }

    for (path, pdl) in &pdls {
        writeln!(
            f,
            r#"{}#[doc(hidden)]
pub const {}_VERSION: &str = "{}.{}";"#,
            Comments(&pdl.description),
            path.file_stem().unwrap().to_str().unwrap().to_uppercase(),
            pdl.version.major,
            pdl.version.minor
        )?;

        let all_domains = env::var_os("CARGO_FEATURE_ALL").is_some();

        for domain in &pdl.domains {
            if !all_domains
                && env::var_os(format!("CARGO_FEATURE_{}", domain.name.to_uppercase())).is_none()
            {
                continue;
            }

            println!("generating domain: {}", domain.name);

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
{}{}{}pub trait {}{} {{
    type Error;
{}}}"#,
                Comments(&domain.description),
                experimental,
                deprecated,
                domain.name,
                if domain.dependencies.is_empty() {
                    "".to_owned()
                } else {
                    format!(": {}", domain.dependencies.join(" + "))
                },
                indented(Trait(domain))
            )?;

            if cfg!(feature = "client") {
                writeln!(
                    f,
                    r#"
{}impl<T> {} for T where T: CallSite {{
    type Error = <T as CallSite>::Error;
{}}}"#,
                    experimental,
                    domain.name,
                    indented(CallSite(domain))
                )?;
            }

            if cfg!(feature = "async") {
                writeln!(
                    f,
                    r#"
{}{}{}pub trait Async{}{} where Self: Sized {{
    type Error;
{}}}"#,
                    Comments(&domain.description),
                    experimental,
                    deprecated,
                    domain.name,
                    if domain.dependencies.is_empty() {
                        "".to_owned()
                    } else {
                        format!(
                            ": {}",
                            domain
                                .dependencies
                                .iter()
                                .map(|name| format!("Async{}", name))
                                .collect::<Vec<_>>()
                                .join(" + ")
                        )
                    },
                    indented(AsyncTrait(domain))
                )?;

                if cfg!(feature = "client") {
                    writeln!(
                        f,
                        r#"
{}impl<T> Async{} for T
where
    T: AsyncCallSite + 'static,
    <T as AsyncCallSite>::Error: 'static,
{{
    type Error = <T as AsyncCallSite>::Error;
{}}}"#,
                        experimental,
                        domain.name,
                        indented(AsyncCallSite(domain))
                    )?;
                }
            }

            writeln!(
                f,
                r#"
{}{}{}#[allow(deprecated)]
pub mod {} {{
{}}}"#,
                Comments(&domain.description),
                experimental,
                deprecated,
                domain.name.to_snake(),
                indented(Mod(domain, |domain, struct_name, ty| has_default(
                    &types,
                    domain,
                    struct_name,
                    ty
                )))
            )?;

            for evt in &domain.events {
                events.push(format!(
                    r#"{}{}{}#[serde(rename = "{}.{}")]
{}({}::{}),"#,
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
                    domain.name,
                    evt.name,
                    if evt
                        .name
                        .to_lowercase()
                        .starts_with(&domain.name.to_lowercase())
                    {
                        evt.name.to_capitalized()
                    } else {
                        format!(
                            "{}{}",
                            domain.name.to_capitalized(),
                            evt.name.to_capitalized()
                        )
                    },
                    domain.name.to_snake(),
                    evt.mangled_name()
                ));
            }
        }
    }

    writeln!(
        f,
        r#"
{}
{}
#[derive(Clone, Debug, PartialEq{}{})]
#[serde(tag = "method", content = "params")]
#[allow(clippy::large_enum_variant)]
pub enum Event {{
{}
}}
"#,
        EVENTS,
        if cfg!(feature = "async") {
            ASYNC_EVENTS
        } else {
            ""
        },
        if cfg!(feature = "server") {
            ", Serialize"
        } else {
            ""
        },
        if cfg!(feature = "client") {
            ", Deserialize"
        } else {
            ""
        },
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

        for cmd in &domain.commands {
            writeln!(
                f,
                "\n{}{}{}fn {}(&mut self{}) -> Result<{}, <Self as {}>::Error>;",
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
                args(domain, cmd),
                returns(domain, cmd),
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
                r#"{}type {}: futures::Future<Item = ({}, Self), Error = <Self as Async{}>::Error>;"#,
                if cmd.experimental {
                    "#[cfg(feature = \"experimental\")]\n"
                } else {
                    ""
                },
                cmd.name.to_capitalized(),
                returns(domain, cmd),
                domain.name,
            )?;
        }

        for cmd in &domain.commands {
            writeln!(
                f,
                "\n{}{}{}fn {}(self{}) -> <Self as Async{}>::{};",
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
                args(domain, cmd),
                domain.name,
                cmd.name.to_capitalized(),
            )?;
        }

        Ok(())
    }
}

struct CallSite<'a>(&'a pdl::Domain<'a>);

impl<'a> fmt::Display for CallSite<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let domain = self.0;

        for cmd in &domain.commands {
            writeln!(
                f,
                r#"
{}fn {}(&mut self{}) -> Result<{}, <Self as {}>::Error> {{
    CallSite::call(self, {}){}
}}"#,
                if cmd.experimental {
                    "#[cfg(feature = \"experimental\")]\n"
                } else {
                    ""
                },
                cmd.name.to_snake(),
                args(domain, cmd),
                returns(domain, cmd),
                domain.name,
                inline_args(domain, cmd),
                inline_returns(cmd, true),
            )?;
        }

        Ok(())
    }
}

struct AsyncCallSite<'a>(&'a pdl::Domain<'a>);

impl<'a> fmt::Display for AsyncCallSite<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let domain = self.0;

        for cmd in &domain.commands {
            writeln!(
                f,
                "{}type {} = Box<dyn futures::Future<Item = ({}, Self), Error = <Self as AsyncCallSite>::Error> + Send>;",
                if cmd.experimental {
                    "#[cfg(feature = \"experimental\")]\n"
                } else {
                    ""
                },
                cmd.name.to_capitalized(),
                returns(domain, cmd),
            )?;
        }

        for cmd in &domain.commands {
            writeln!(
                f,
                r#"
{}fn {}(self{}) -> <Self as Async{}>::{} {{
    {}(AsyncCallSite::async_call(self, {}){})
}}"#,
                if cmd.experimental {
                    "#[cfg(feature = \"experimental\")]\n"
                } else {
                    ""
                },
                cmd.name.to_snake(),
                args(domain, cmd),
                domain.name,
                cmd.name.to_capitalized(),
                if cmd.returns.len() <= MAX_INLINE_RETURNS {
                    "Box::new"
                } else {
                    ""
                },
                inline_args(domain, cmd),
                inline_returns(cmd, false)
            )?;
        }

        Ok(())
    }
}

fn args(domain: &pdl::Domain, cmd: &pdl::Command) -> String {
    if cmd.parameters.len() <= MAX_INLINE_PARAMS {
        cmd.parameters
            .iter()
            .map(|param| {
                let mut ty = match param.ty {
                    pdl::Type::Enum(_) => format!(
                        "{}::{}Request{}",
                        domain.name.to_snake(),
                        cmd.name.to_capitalized(),
                        param.name.to_capitalized()
                    ),
                    _ => Type(&param.ty, None, Some(domain.name)).to_string(),
                };

                if param.optional {
                    ty = format!("Option<{}>", ty)
                }

                format!(", {}: {}", param.mangled_name(), ty)
            })
            .collect::<Vec<_>>()
            .join("")
    } else {
        format!(
            ", req: {}::{}Request",
            domain.name.to_snake(),
            cmd.name.to_capitalized()
        )
    }
}

fn returns(domain: &pdl::Domain, cmd: &pdl::Command) -> String {
    if cmd.returns.len() <= MAX_INLINE_RETURNS {
        format!(
            "({})",
            cmd.returns
                .iter()
                .map(|param| {
                    let mut ty = match param.ty {
                        pdl::Type::Enum(_) => format!(
                            "{}::{}Response{}",
                            domain.name.to_snake(),
                            cmd.name.to_capitalized(),
                            param.name.to_capitalized()
                        ),
                        _ => Type(&param.ty, None, Some(domain.name)).to_string(),
                    };

                    if param.optional {
                        ty = format!("Option<{}>", ty)
                    }

                    ty.to_string()
                })
                .collect::<Vec<_>>()
                .join(", ")
        )
    } else {
        format!(
            "{}::{}Response",
            domain.name.to_snake(),
            cmd.name.to_capitalized()
        )
    }
}

fn inline_args(domain: &pdl::Domain, cmd: &pdl::Command) -> String {
    match cmd.parameters.len() {
        0 => format!(
            "{}::{}Request {{}}",
            domain.name.to_snake(),
            cmd.name.to_capitalized()
        ),
        n if n <= MAX_INLINE_PARAMS => format!(
            "{}::{}Request {{ {} }}",
            domain.name.to_snake(),
            cmd.name.to_capitalized(),
            cmd.parameters
                .iter()
                .map(MangledName::mangled_name)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        _ => "req".to_owned(),
    }
}

fn inline_returns(cmd: &pdl::Command, sync: bool) -> String {
    match cmd.returns.len() {
        0 => if sync {
            ".map(|_| ())"
        } else {
            ".map(|(_, self_)| ((), self_))"
        }
        .to_owned(),
        n if n <= MAX_INLINE_RETURNS => {
            let returns = cmd
                .returns
                .iter()
                .map(|param| format!("res.{}", param.name.to_snake()))
                .collect::<Vec<_>>()
                .join(", ");

            if sync {
                format!(".map(|res| ({}))", returns)
            } else {
                format!(".map(|(res, self_)| (({}), self_))", returns)
            }
        }
        _ => "".to_owned(),
    }
}

struct Mod<'a, F>(&'a pdl::Domain<'a>, F);

impl<'a, F> fmt::Display for Mod<'a, F>
where
    F: Fn(&'a pdl::Domain<'a>, Option<&'a str>, &'a pdl::Type<'a>) -> bool,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let domain = self.0;
        let has_default = &self.1;
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
                        Struct(
                            ty.id.to_owned(),
                            props,
                            |name| has_default(domain, Some(ty.id), name),
                            true,
                            true
                        )
                    )?;
                }
                None => {
                    writeln!(
                        f,
                        "pub type {} = {};",
                        ty.id,
                        Type(&ty.extends, Some(ty.id), None)
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
                Struct(
                    request,
                    &cmd.parameters,
                    |name| has_default(domain, None, name),
                    cfg!(feature = "client"),
                    cfg!(feature = "server")
                )
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
                Struct(
                    response,
                    &cmd.returns,
                    |name| has_default(domain, None, name),
                    cfg!(feature = "server"),
                    cfg!(feature = "client")
                )
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

            let mangled_name = evt.mangled_name();

            events.push(format!(
                r#"{}{}{}#[serde(rename = "{}.{}")]
{}({}),"#,
                Comments(&evt.description),
                experimental,
                deprecated,
                domain.name,
                evt.name,
                evt.name.to_capitalized(),
                mangled_name,
            ));

            writeln!(
                f,
                r#"{}{}{}{}"#,
                Comments(&evt.description),
                experimental,
                deprecated,
                Struct(
                    mangled_name,
                    &evt.parameters,
                    |name| has_default(domain, None, name),
                    cfg!(feature = "server"),
                    cfg!(feature = "client")
                )
            )?;
        }

        if !events.is_empty() {
            writeln!(
                f,
                r#"{}
{}
#[derive(Clone, Debug, PartialEq{}{})]
#[serde(tag = "method", content = "params")]
#[allow(clippy::large_enum_variant)]
pub enum Event {{
{}
}}"#,
                EVENTS,
                if cfg!(feature = "async") {
                    ASYNC_EVENTS
                } else {
                    ""
                },
                if cfg!(feature = "server") {
                    ", Serialize"
                } else {
                    ""
                },
                if cfg!(feature = "client") {
                    ", Deserialize"
                } else {
                    ""
                },
                indented(events.join("\n\n"))
            )?;
        }

        Ok(())
    }
}

struct Type<'a>(&'a pdl::Type<'a>, Option<&'a str>, Option<&'a str>);

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
            pdl::Type::Enum(_) => unreachable!(),
            pdl::Type::ArrayOf(ty) => write!(f, "Vec<{}>", Type(&ty, None, self.2)),
            pdl::Type::Ref(id) => {
                let ref_ty = if id.contains('.') {
                    let mut parts = id.split('.');

                    let domain = parts.next().unwrap();
                    let name = parts.next().unwrap();

                    format!("{}::{}", domain.to_snake(), name.to_capitalized())
                } else if let Some(domain) = self.2 {
                    format!("{}::{}", domain.to_snake(), id.to_capitalized())
                } else {
                    id.to_capitalized()
                };

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

struct Struct<'a, F>(String, &'a [pdl::Param<'a>], F, bool, bool);

impl<'a, F> fmt::Display for Struct<'a, F>
where
    F: Fn(&'a pdl::Type<'a>) -> bool,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = &self.0;
        let params = self.1;
        let has_default = &self.2;
        let has_serialize = self.3;
        let has_deserialize = self.4;

        writeln!(
            f,
            r#"#[derive(Clone, Debug{}, PartialEq{}{})]
#[serde(rename_all = "camelCase")]
pub struct {} {{"#,
            if params
                .iter()
                .all(|param| param.optional || has_default(&param.ty))
            {
                ", Default"
            } else {
                ""
            },
            if has_serialize { ", Serialize" } else { "" },
            if has_deserialize { ", Deserialize" } else { "" },
            name
        )?;

        for prop in params {
            writeln!(f, "{},", indented(Field(&prop, &name)))?;
        }

        writeln!(f, "}}")?;

        for prop in params {
            if let pdl::Type::Enum(ref variants) = prop.ty {
                write!(
                    f,
                    "\n/// Allow values for the `{}::{}` field.\n{}{}",
                    name,
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
        let mangled_name = param.mangled_name();

        write!(
            f,
            "{}{}{}{}{}pub {}: ",
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
            if param.optional {
                "#[serde(skip_serializing_if = \"Option::is_none\")]\n"
            } else if let pdl::Type::ArrayOf(_) = param.ty {
                "#[serde(skip_serializing_if = \"Vec::is_empty\")]\n"
            } else {
                ""
            },
            mangled_name
        )?;

        let ty = if let pdl::Type::Enum(_) = param.ty {
            format!("{}{}", self.1, param.name.to_capitalized())
        } else {
            Type(&param.ty, Some(self.1), None).to_string()
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

trait MangledName {
    fn mangled_name(&self) -> String;
}

impl MangledName for pdl::Event<'_> {
    fn mangled_name(&self) -> String {
        let name = self.name.to_capitalized();

        if name.ends_with("Event") {
            name
        } else {
            format!("{}Event", name.to_capitalized())
        }
    }
}

impl MangledName for pdl::Param<'_> {
    fn mangled_name(&self) -> String {
        let name = self.name.replace('-', "").to_snake();

        if KEYWORDS.contains(&name.as_str()) {
            format!("r#{}", name)
        } else {
            name
        }
    }
}

fn main() -> Result<(), Error> {
    gen(&[
        "devtools-protocol/pdl/browser_protocol.pdl",
        "devtools-protocol/pdl/js_protocol.pdl",
    ])?;

    Ok(())
}
