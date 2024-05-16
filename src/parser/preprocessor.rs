use anyhow::anyhow;
use anyhow::Result;
use pest::iterators::*;
use pest::Parser;
use std::fmt::Write;

pub struct PreprocessorContext {
    pub use_rtdbg: bool,
}

pub type IntrinsicMacroDecl = fn(args: &[String], ctx: &PreprocessorContext) -> String;

pub enum MacroDeclMode {
    UserDefined {
        args: Vec<String>,
        text: String,
    },
    Intrinsic(IntrinsicMacroDecl),
}

pub struct MacroDecl {
    pub mode: MacroDeclMode,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MacroCall {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(pest_derive::Parser)]
#[grammar = "./parser/preprocessor.pest"]
struct SyntaxParser;

fn parse_macro_decl(mut pairs: Pairs<Rule>) -> Result<MacroDecl> {
    pairs.next(); // skip "macro" keyword
    let name = pairs.next().unwrap().as_str().trim().to_owned();

    let mut next = pairs.next().unwrap();
    let mut args = Vec::new();
    if next.as_rule() == Rule::macro_args {
        let args_pairs = next.into_inner();
        for arg in args_pairs {
            args.push(arg.as_str().trim().to_owned());
        }

        next = pairs.next().unwrap();
    }
    let text = next.as_str().to_owned();

    Ok(MacroDecl {
        name,
        mode: MacroDeclMode::UserDefined {
            args,
            text,
        },
    })
}

fn parse_macro_call(mut pairs: Pairs<Rule>) -> Result<MacroCall> {
    let name = pairs.next().unwrap().as_str().trim().to_owned();
    let mut args = Vec::new();

    for arg in pairs {
        let as_str = arg.as_str();
        args.push(match arg.as_rule() {
            Rule::marked_macro_param => as_str["```".len()..as_str.len() - "```".len()].trim().to_owned(),
            Rule::simple_macro_param => as_str.to_owned(),
            x => unimplemented!("{:?}", x),
        });
    }

    Ok(MacroCall {
        name,
        args,
    })
}

fn intrinsic_macro_decl_if(args: &[String], ctx: &PreprocessorContext) -> String {
    if args.len() != 2 {
        panic!("macro $IF takes 2 arguments");
    }

    if args[0] == "RTDBG" {
        if ctx.use_rtdbg {
            return args[1].clone();
        }
        String::new()
    } else if args[0] == "true" {
        args[1].clone()
    } else if args[0] == "false" {
        String::new()
    } else {
        panic!("not a boolean variable: {}", args[0])
    }
}

fn parse_all_macro_decls(code: &str) -> Result<Vec<MacroDecl>> {
    let result = SyntaxParser::parse(Rule::program, code);
    match result {
        Ok(mut pairs) => match pairs.next() {
            Some(pair) => Ok({
                let pairs = pair.into_inner();
                let mut macro_decls = Vec::new();

                macro_decls.push(MacroDecl {
                    name: "$IF".to_owned(),
                    mode: MacroDeclMode::Intrinsic(intrinsic_macro_decl_if),
                });

                for next in pairs {
                    match next.as_rule() {
                        Rule::macro_decl => macro_decls.push(parse_macro_decl(next.into_inner())?),
                        _ => (),
                    }
                }
                macro_decls
            }),
            None => Err(anyhow!("no input provided")),
        },
        Err(msg) => Err(anyhow!("Preprocessor panic: {}", msg)),
    }
}

fn parse_program(pairs: Pairs<Rule>, macro_decls: &[MacroDecl], ctx: &PreprocessorContext) -> Result<String> {
    let mut processed = String::new();
    for next in pairs {
        match next.as_rule() {
            Rule::macro_decl => (),
            Rule::macro_call => {
                let call = parse_macro_call(next.into_inner())?;
                let decl = match macro_decls.iter().find(|decl| decl.name == call.name) {
                    Some(decl) => decl,
                    None => panic!("no such macro `{}(...)`", call.name),
                };

                match &decl.mode {
                    MacroDeclMode::Intrinsic(intrinsic) => write!(&mut processed, "{}\n\n", intrinsic(&call.args, ctx))?,
                    MacroDeclMode::UserDefined {
                        args,
                        text,
                    } => {
                        if args.len() != call.args.len() {
                            panic!("expecting {} args for macro `{}(....)`, but received {}", args.len(), call.name, call.args.len());
                        }

                        let mut replaced = text[0..text.len() - "end macro".len()].trim().to_owned();
                        for i in 0..args.len() {
                            replaced = replaced.replace(&args[i], &call.args[i]);
                        }

                        write!(&mut processed, "{}\n\n", replaced)?
                    }
                }
            }
            Rule::raw_text => {
                write!(&mut processed, "{}\n\n", next.as_str())?;
            }
            Rule::EOI => return Ok(processed),
            x => unreachable!("{:#?}", x),
        }
    }

    Ok(processed)
}

fn preprocess_round(code: &str, decls: &[MacroDecl], ctx: &PreprocessorContext) -> Result<String> {
    let result = SyntaxParser::parse(Rule::program, code);
    match result {
        Ok(mut pairs) => match pairs.next() {
            Some(pair) => Ok(parse_program(pair.into_inner(), decls, ctx)?),
            None => Err(anyhow!("no input provided")),
        },
        Err(msg) => Err(anyhow!("Preprocessor panic: {}", msg)),
    }
}

pub fn preprocess(code: &str, ctx: &PreprocessorContext) -> Result<String> {
    // Recursively apply preprocessing until all mecro invocations have been resolved.
    let mut current_code = code.to_owned();
    let decls = parse_all_macro_decls(code)?;
    for _ in 0..8 {
        let next_code = preprocess_round(&current_code, &decls, ctx)?;
        if current_code == next_code {
            return Ok(next_code);
        }
        current_code = next_code;
    }
    Err(anyhow!("Preprocessor panic: maximum recursion depth (8) reached"))
}
