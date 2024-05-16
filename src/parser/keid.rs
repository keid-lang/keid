use crate::{tree::ast::*, tree::*};
use anyhow::{anyhow, Result};
use lazy_static::lazy_static;
use pest::{error::*, iterators::*, pratt_parser::*, Parser, Span};
use std::{fmt::Write, path::Path};

use super::preprocessor::{self, PreprocessorContext};

#[derive(pest_derive::Parser)]
#[grammar = "./parser/keid.pest"]
struct SyntaxParser;

lazy_static! {
    static ref PRATT: PrattParser<Rule> = PrattParser::new()
        .op(Op::infix(Rule::op_null_coalesce, Assoc::Left))
        .op(Op::infix(Rule::op_or, Assoc::Left))
        .op(Op::infix(Rule::op_and, Assoc::Left))
        .op(Op::infix(Rule::op_bit_or, Assoc::Left))
        .op(Op::infix(Rule::op_bit_xor, Assoc::Left))
        .op(Op::infix(Rule::op_bit_and, Assoc::Left))
        .op(Op::infix(Rule::op_equal, Assoc::Left) | Op::infix(Rule::op_not_equal, Assoc::Left))
        .op(Op::infix(Rule::op_lt, Assoc::Left)
            | Op::infix(Rule::op_lte, Assoc::Left)
            | Op::infix(Rule::op_gt, Assoc::Left)
            | Op::infix(Rule::op_gte, Assoc::Left))
        .op(Op::infix(Rule::op_shl, Assoc::Left) | Op::infix(Rule::op_shr, Assoc::Left))
        .op(Op::infix(Rule::op_add, Assoc::Left) | Op::infix(Rule::op_sub, Assoc::Left))
        .op(Op::infix(Rule::op_mul, Assoc::Left) | Op::infix(Rule::op_div, Assoc::Left) | Op::infix(Rule::op_rem, Assoc::Left))
        .op(Op::infix(Rule::op_as, Assoc::Left))
        .op(Op::prefix(Rule::op_not))
        .op(Op::prefix(Rule::op_spread))
        .op(Op::prefix(Rule::op_negate))
        .op(Op::postfix(Rule::op_null_assert));
}

pub fn span_to_location(span: &Span) -> TokenLocation {
    TokenLocation {
        start: span.start(),
        end: span.end(),
    }
}

fn tokenize<T: Clone + PartialEq>(span: &Span, comp: T) -> Token<T> {
    Token {
        token: comp,
        loc: span_to_location(span),
    }
}

fn parse_import(mut pairs: Pairs<Rule>) -> Result<Vec<Qualifier>> {
    pairs.next(); // skip import keyword

    let mut qualifiers = Vec::new();
    for pair in pairs {
        qualifiers.push(Qualifier::from_idents(pair));
    }

    Ok(qualifiers)
}

enum StringParserState {
    Normal,
    Escape,
}

fn parse_string_lit(str: Pair<Rule>) -> String {
    let raw = str.as_str();
    let raw = &raw[1..raw.len() - 1];

    let mut parser_state = StringParserState::Normal;
    let mut parsed_str = String::new();
    for c in raw.chars() {
        match parser_state {
            StringParserState::Normal => match c {
                '\\' => {
                    parser_state = StringParserState::Escape;
                }
                c => {
                    write!(&mut parsed_str, "{c}").unwrap();
                }
            },
            StringParserState::Escape => {
                let m = match c {
                    '"' => '"',
                    '\'' => '\'',
                    '\\' => '\\',
                    '0' => '\0',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    c => panic!("Invalid escape sequence '\\{}'", c),
                };
                write!(&mut parsed_str, "{m}").unwrap();
                parser_state = StringParserState::Normal;
            }
        }
    }

    parsed_str
}

fn parse_char_lit(str: Pair<Rule>) -> char {
    let str = parse_string_lit(str);
    if str.len() != 1 {
        panic!("invalid char constant '{}'", str);
    }
    str.chars().next().unwrap()
}

fn parse_sint_lit(int: Pair<Rule>) -> Result<i64> {
    let mut radix = 10;
    let mut str = int.as_str().to_owned();
    if str.starts_with("-0x") {
        radix = 16;
        str = "-".to_owned() + &str[3..];
    } else if str.starts_with("0x") {
        radix = 16;
        str = str[2..].to_owned();
    } else if str.starts_with("0o") {
        radix = 8;
        str = str[2..].to_owned();
    }
    Ok(i64::from_str_radix(str.as_str(), radix).map_err(|_| {
        Error::new_from_span(
            ErrorVariant::<Rule>::CustomError {
                message: "invalid integer literal".to_owned(),
            },
            int.as_span(),
        )
    })?)
}

fn parse_member(pairs: Pairs<Rule>) -> Result<MemberExpr> {
    let mut members = Vec::new();
    let mut namespace = None;
    for pair in pairs {
        let (ty, member) = match pair.as_rule() {
            Rule::initial_member_expr => {
                let mut inner = pair.into_inner();
                match inner.peek().unwrap().as_rule() {
                    Rule::literal_qualifier => {
                        namespace = Some(Qualifier::from_idents(inner.next().unwrap()));
                    }
                    _ => (),
                }
                let mut root = parse_expr(inner.next().unwrap())?;
                for postfix_pair in inner {
                    root = parse_postfix_expr(root, postfix_pair)?;
                }
                (MemberType::Root, root)
            }
            Rule::class_member => (MemberType::Class, parse_expr(pair.into_inner().next().unwrap())?),
            Rule::array_member => (MemberType::Array, parse_expr(pair.into_inner().next().unwrap())?),
            x => unimplemented!("{:?}", x),
        };
        members.push(Member {
            ty,
            value: member,
        });
    }

    Ok(MemberExpr {
        prefix: namespace,
        members,
    })
}

fn parse_reference(mut pairs: Pairs<Rule>) -> Result<Token<Expr>> {
    // same behavior for both ref and deref

    pairs.next(); // skip "ref" / "deref" keyword
    parse_expr(pairs.next().unwrap())
}

fn parse_range(mut pairs: Pairs<Rule>) -> Result<Range> {
    let start = parse_expr(pairs.next().unwrap())?;
    let end = parse_expr(pairs.next().unwrap())?;

    Ok(Range {
        start: Box::new(start),
        end: Box::new(end),
    })
}

fn parse_new_array(mut pairs: Pairs<Rule>) -> Result<NewArray> {
    pairs.next(); // skip "new" keyword
    let element_type = QualifiedType::from_idents(pairs.next().unwrap());
    let initial_value = parse_expr(pairs.next().unwrap())?;
    let length = parse_expr(pairs.next().unwrap())?;

    Ok(NewArray {
        element_type,
        initial_value: Box::new(initial_value),
        length: Box::new(length),
    })
}

fn parse_specified_array(mut pairs: Pairs<Rule>) -> Result<SpecifiedArray> {
    pairs.next(); // skip "new" keyword
    let element_type = QualifiedType::from_idents(pairs.next().unwrap());
    let mut initial_values = Vec::new();

    for next in pairs {
        initial_values.push(parse_expr(next)?);
    }

    Ok(SpecifiedArray {
        element_type,
        initial_values,
    })
}

fn parse_unary_type_expr(mut pairs: Pairs<Rule>) -> QualifiedType {
    pairs.next(); // skip keyword
    QualifiedType::from_idents(pairs.next().unwrap())
}

fn parse_postfix_expr(operand: Token<Expr>, operator: Pair<Rule>) -> Result<Token<Expr>> {
    Ok(Token {
        loc: TokenLocation {
            start: operator.as_span().start(),
            end: operand.loc.end,
        },
        token: Expr::Unary(UnaryExpr {
            value: Box::new(operand),
            op: match operator.as_rule() {
                Rule::op_null_assert => Operator::NonNullAssertion,
                x => unimplemented!("{:#?}", x),
            },
        }),
    })
}

fn parse_logic_expr(pairs: Pairs<Rule>) -> Result<Token<Expr>> {
    PRATT
        .map_primary(parse_expr)
        .map_prefix(|operator, operand| {
            let operand = operand?;
            Ok(Token {
                loc: TokenLocation {
                    start: operator.as_span().start(),
                    end: operand.loc.end,
                },
                token: Expr::Unary(UnaryExpr {
                    value: Box::new(operand),
                    op: match operator.as_rule() {
                        Rule::op_not => Operator::Not,
                        Rule::op_spread => Operator::Spread,
                        Rule::op_negate => Operator::Negate,
                        x => unimplemented!("{:#?}", x),
                    },
                }),
            })
        })
        .map_postfix(|operand, operator| {
            let operand = operand?;
            parse_postfix_expr(operand, operator)
        })
        .map_infix(|lhs, op, rhs| {
            let lhs = lhs?;
            let rhs = rhs?;
            Ok(Token {
                loc: TokenLocation {
                    start: lhs.loc.start,
                    end: rhs.loc.end,
                },
                token: Expr::Logic(LogicExpr {
                    op: match op.as_rule() {
                        Rule::op_equal => Operator::Equals,
                        Rule::op_not_equal => Operator::NotEquals,
                        Rule::op_add => Operator::Add,
                        Rule::op_sub => Operator::Subtract,
                        Rule::op_mul => Operator::Multiply,
                        Rule::op_div => Operator::Divide,
                        Rule::op_rem => Operator::Modulus,
                        Rule::op_shl => Operator::LeftShift,
                        Rule::op_shr => Operator::RightShift,
                        Rule::op_or => Operator::BooleanOr,
                        Rule::op_and => Operator::BooleanAnd,
                        Rule::op_lte => Operator::LessThanOrEquals,
                        Rule::op_gte => Operator::GreaterThanOrEquals,
                        Rule::op_lt => Operator::LessThan,
                        Rule::op_gt => Operator::GreaterThan,
                        Rule::op_as => Operator::As,
                        Rule::op_bit_and => Operator::BitAnd,
                        Rule::op_bit_or => Operator::BitOr,
                        Rule::op_bit_xor => Operator::BitXor,
                        x => unreachable!("{:?}", x),
                    },
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }),
            })
        })
        .parse(pairs)
}

fn parse_enum_with_data(mut pairs: Pairs<Rule>) -> Result<EnumWithDataExpr> {
    pairs.next(); // skip "new" keyword
    let declaring_type = Qualifier::from_idents(pairs.next().unwrap());
    let member = Identifier::from_ident(&pairs.next().unwrap());
    let data = parse_anonymous_struct(pairs.next().unwrap().into_inner())?;

    Ok(EnumWithDataExpr {
        declaring_type,
        member,
        data,
    })
}

fn parse_expr(pair: Pair<Rule>) -> Result<Token<Expr>> {
    let span = pair.as_span();
    let token = match pair.as_rule() {
        Rule::reference_op => Expr::Reference(Box::new(parse_reference(pair.into_inner())?)),
        Rule::dereference_op => Expr::Dereference(Box::new(parse_reference(pair.into_inner())?)),
        Rule::null => Expr::Null,
        Rule::boolean => Expr::BoolLit(pair.as_str() == "true"),
        Rule::string => Expr::StringLit(parse_string_lit(pair)),
        Rule::char => Expr::CharLit(parse_char_lit(pair)),
        Rule::integer => Expr::SignedIntLit(parse_sint_lit(pair)?),
        Rule::enum_with_data_ref => Expr::EnumWithData(parse_enum_with_data(pair.into_inner())?),
        Rule::func_call => Expr::FuncCall(parse_func_call(pair.into_inner())?),
        Rule::ident => Expr::Ident(Identifier::from_ident(&pair)),
        Rule::member => Expr::Member(parse_member(pair.into_inner())?),
        Rule::new_call => parse_new_call(pair.into_inner())?,
        Rule::logic_expr | Rule::member_expr => return parse_logic_expr(pair.into_inner()),
        Rule::variable_type => Expr::CastTarget(QualifiedType::from_idents(pair)),
        Rule::range => Expr::Range(parse_range(pair.into_inner())?),
        Rule::new_array => Expr::NewArray(parse_new_array(pair.into_inner())?),
        Rule::specified_array => Expr::SpecifiedArray(parse_specified_array(pair.into_inner())?),
        Rule::default_expr => Expr::Default(parse_unary_type_expr(pair.into_inner())),
        Rule::sizeof_expr => Expr::SizeOf(parse_unary_type_expr(pair.into_inner())),
        Rule::anonymous_struct => Expr::AnonymousStruct(parse_anonymous_struct(pair.into_inner())?),
        Rule::match_expr => Expr::Match(parse_match_expr(pair.into_inner())?),
        Rule::array_slice_expr => Expr::Subslice(parse_subslice_expr(pair.into_inner())?),
        x => unimplemented!("[{:?}]: {:?}", x, pair),
    };
    Ok(tokenize(&span, token))
}

fn parse_subslice_expr(mut pairs: Pairs<Rule>) -> Result<SubsliceExpr> {
    let mut start = None;
    let mut end = None;
    let mut crossed_separator = false;
    loop {
        match pairs.next() {
            Some(pair) => {
                if pair.as_rule() == Rule::array_slice_separator {
                    crossed_separator = true;
                } else {
                    if crossed_separator {
                        end = Some(Box::new(parse_expr(pair)?));
                    } else {
                        start = Some(Box::new(parse_expr(pair)?));
                    }
                }
            }
            None => break,
        }
    }
    Ok(SubsliceExpr {
        start,
        end,
    })
}

fn parse_match_expr(mut pairs: Pairs<Rule>) -> Result<MatchExpr> {
    pairs.next(); // skip "match" keyword

    let value = parse_expr(pairs.next().unwrap())?;
    let mut branches = Vec::new();
    for next in pairs {
        let mut stmt = next.into_inner();
        let arg = stmt.next().unwrap();
        let arg = match arg.as_rule() {
            Rule::inner_match_catchall => MatchExprBranchArg::Catchall(span_to_location(&arg.as_span())),
            Rule::inner_match_enum => {
                let mut inner_enum = arg.into_inner();
                let member = Identifier::from_ident(&inner_enum.next().unwrap());
                let data_type = parse_anonymous_struct_type_decl(inner_enum.next().unwrap().into_inner());
                MatchExprBranchArg::EnumWithData {
                    member,
                    data_type,
                }
            }
            Rule::ident => MatchExprBranchArg::Enum(Identifier::from_ident(&arg)),
            _ => MatchExprBranchArg::Expr(parse_expr(arg.clone())?),
        };
        let statement = parse_statement(stmt.next().unwrap())?;

        branches.push(MatchExprBranch {
            arg,
            statement,
        })
    }

    Ok(MatchExpr {
        value: Box::new(value),
        branches,
    })
}

fn parse_let_binding(mut pairs: Pairs<Rule>) -> Result<LetBinding> {
    let name_pair = pairs.next().unwrap();
    let name = Identifier::from_ident(&name_pair);

    let (var_type, initial_value) = {
        let next = match pairs.next() {
            Some(next) => next,
            _ => {
                return Err(Error::<Rule>::new_from_span(
                    ErrorVariant::CustomError {
                        message: "Let statement must have either an explicit type or an initial value, or both".to_string(),
                    },
                    name_pair.as_span(),
                )
                .into())
            }
        };
        match next.as_rule() {
            Rule::variable_type => {
                let var_type = QualifiedType::from_idents(next);
                let initial_value = match pairs.next().map(parse_expr) {
                    Some(Ok(val)) => Some(val),
                    Some(Err(e)) => return Err(e),
                    None => None,
                };
                (Some(var_type), initial_value)
            }
            _ => (None, Some(parse_expr(next)?)),
        }
    };

    Ok(LetBinding {
        name,
        var_type,
        initial_value,
    })
}

fn parse_let(mut pairs: Pairs<Rule>) -> Result<Let> {
    let is_extern = if pairs.peek().unwrap().as_rule() == Rule::keyword_extern {
        pairs.next();
        true
    } else {
        false
    };

    let is_const = pairs.next().unwrap().as_rule() == Rule::keyword_const; // either "let" or "const"
    let mut bindings = Vec::new();

    while let Some(pair) = pairs.next() {
        bindings.push(parse_let_binding(pair.into_inner())?);
    }

    Ok(Let {
        is_extern,
        is_const,
        bindings,
    })
}

fn parse_field_decl(mut pairs: Pairs<Rule>) -> Result<Let> {
    let name = Identifier::from_ident(&pairs.next().unwrap());
    let var_type = Some(QualifiedType::from_idents(pairs.next().unwrap()));
    let initial_value = match pairs.next().map(parse_expr) {
        Some(Ok(val)) => Some(val),
        Some(Err(e)) => return Err(e),
        None => None,
    };
    Ok(Let {
        is_extern: false,
        is_const: false,
        bindings: vec![LetBinding {
            name,
            var_type,
            initial_value,
        }],
    })
}

fn parse_return(mut pairs: Pairs<Rule>) -> Result<Option<Token<Expr>>> {
    pairs.next(); // skip return keyword
    if let Some(expr) = pairs.next() {
        Ok(Some(parse_expr(expr)?))
    } else {
        Ok(None)
    }
}

fn parse_func_call(mut pairs: Pairs<Rule>) -> Result<FuncCall> {
    let callee = parse_expr(pairs.next().unwrap())?;
    let generic_args = parse_generic_args(&mut pairs)?;

    let mut args = Vec::new();
    for arg in pairs {
        args.push(parse_expr(arg)?);
    }

    Ok(FuncCall {
        callee: Box::new(callee),
        generic_args,
        args,
    })
}

fn parse_anonymous_struct(pairs: Pairs<Rule>) -> Result<Vec<NewCallField>> {
    let mut args = Vec::new();
    for arg in pairs {
        match arg.as_rule() {
            Rule::new_call_field => {
                let mut field = arg.into_inner();
                args.push(NewCallField {
                    field_name: Identifier::from_ident(&field.next().unwrap()),
                    value: Some(parse_expr(field.next().unwrap())?),
                })
            }
            Rule::ident => args.push(NewCallField {
                field_name: Identifier::from_ident(&arg),
                value: None,
            }),
            x => unimplemented!("{:?}", x),
        }
    }
    Ok(args)
}

fn parse_new_call(mut pairs: Pairs<Rule>) -> Result<Expr> {
    pairs.next(); // skip "new" keyword
    let type_token = pairs.next().unwrap();
    let ty = QualifiedType::from_idents(type_token);
    let args = parse_anonymous_struct(pairs)?;

    Ok(Expr::New(NewCall {
        ty,
        args,
    }))
}

fn parse_assign(mut pairs: Pairs<Rule>) -> Result<Assign> {
    let deref = if pairs.peek().unwrap().as_rule() == Rule::keyword_deref {
        pairs.next();
        true
    } else {
        false
    };
    Ok(Assign {
        deref,
        lhs: parse_expr(pairs.next().unwrap())?,
        op: match pairs.next().unwrap().as_rule() {
            Rule::op_set_self => Operator::Equals,
            Rule::op_add_self => Operator::Add,
            Rule::op_sub_self => Operator::Subtract,
            Rule::op_mul_self => Operator::Multiply,
            Rule::op_div_self => Operator::Divide,
            _ => todo!(),
        },
        rhs: parse_expr(pairs.next().unwrap())?,
    })
}

fn parse_if_chain(mut pairs: Pairs<Rule>) -> Result<IfChain> {
    let mut chain = IfChain {
        conditionals: Vec::new(),
        fallback: None,
    };

    loop {
        let first = match pairs.next() {
            Some(pair) => pair,
            None => break,
        };
        match first.as_rule() {
            Rule::keyword_if => {
                // conditional block
                let test = parse_expr(pairs.next().unwrap())?;
                let body = parse_block(pairs.next().unwrap())?;
                if pairs.peek().is_some() {
                    pairs.next(); // skip the "else" if it's present
                }
                chain.conditionals.push(ConditionalBlock {
                    test: Box::new(test),
                    body,
                });
            }
            Rule::block => {
                // else block
                chain.fallback = Some(parse_block(first)?);
                break;
            }
            _ => (),
        }
    }

    Ok(chain)
}

fn parse_for_loop(mut pairs: Pairs<Rule>) -> Result<ForLoop> {
    pairs.next(); // skip "for" keyword
    let variable = Identifier::from_ident(&pairs.next().unwrap());
    pairs.next(); // skip "in" keyword
    let iterator = parse_expr(pairs.next().unwrap())?;
    let block = parse_block(pairs.next().unwrap())?;

    Ok(ForLoop {
        variable,
        iterator,
        block,
    })
}

fn parse_while_loop(mut pairs: Pairs<Rule>) -> Result<WhileLoop> {
    pairs.next(); // skip "while" keyword
    let condition = parse_expr(pairs.next().unwrap())?;
    let block = parse_block(pairs.next().unwrap())?;

    Ok(WhileLoop {
        condition,
        block,
    })
}

fn parse_throw_statement(mut pairs: Pairs<Rule>) -> Result<Token<Expr>> {
    pairs.next(); // skip "throw" keyword

    parse_expr(pairs.next().unwrap())
}

fn parse_indefinite_loop(mut pairs: Pairs<Rule>) -> Result<Vec<Token<Statement>>> {
    pairs.next(); // skip "loop" keyword

    parse_block(pairs.next().unwrap())
}

fn parse_unsafe_block(mut pairs: Pairs<Rule>) -> Result<Vec<Token<Statement>>> {
    pairs.next(); // skip "unsafe" keyword

    let block = parse_block(pairs.next().unwrap())?;
    Ok(block)
}

fn parse_with_block(mut pairs: Pairs<Rule>) -> Result<WithBlock> {
    pairs.next(); // skip "with" keyword

    let variable = parse_let(pairs.next().unwrap().into_inner())?;
    let block = parse_block(pairs.next().unwrap())?;
    Ok(WithBlock {
        variable,
        block,
    })
}

fn parse_try_catch_statement(mut pairs: Pairs<Rule>) -> Result<TryCatch> {
    pairs.next(); // skip "try" keyword
    let try_block = parse_block(pairs.next().unwrap())?;
    pairs.next(); // skip "catch" keyword
    let error_var = Identifier::from_ident(&pairs.next().unwrap());
    let catch_block = parse_block(pairs.next().unwrap())?;

    Ok(TryCatch {
        try_block,
        error_var: Some(error_var),
        catch_block,
    })
}

fn parse_statement(pair: Pair<Rule>) -> Result<Token<Statement>> {
    let inner = pair.clone().into_inner();
    Ok(tokenize(
        &pair.as_span(),
        match pair.as_rule() {
            Rule::let_statement => Statement::Let(parse_let(inner)?),
            Rule::return_statement => Statement::Return(parse_return(inner)?),
            Rule::assign_statement => Statement::Assign(parse_assign(inner)?),
            Rule::if_statement => Statement::IfChain(parse_if_chain(inner)?),
            Rule::for_loop => Statement::ForLoop(parse_for_loop(inner)?),
            Rule::while_loop => Statement::WhileLoop(parse_while_loop(inner)?),
            Rule::block => Statement::Block(parse_block(pair)?),
            Rule::unsafe_block => Statement::UnsafeBlock(parse_unsafe_block(inner)?),
            Rule::with_block => Statement::WithBlock(parse_with_block(inner)?),
            Rule::indefinite_loop => Statement::IndefiniteLoop(parse_indefinite_loop(inner)?),
            Rule::unreachable_statement => Statement::Unreachable,
            Rule::throw_statement => Statement::Throw(parse_throw_statement(inner)?),
            Rule::try_statement => Statement::TryCatch(parse_try_catch_statement(inner)?),
            Rule::continue_statement => Statement::Continue,
            Rule::break_statement => Statement::Break,
            _ => Statement::Expr(parse_expr(pair)?),
        },
    ))
}

fn parse_block(pair: Pair<Rule>) -> Result<Vec<Token<Statement>>> {
    if pair.as_rule() == Rule::arrow_expr {
        let expr = pair.into_inner().next().unwrap();
        return Ok(vec![tokenize(&expr.as_span(), Statement::ArrowExpr(parse_expr(expr)?))]);
    }

    let pairs = pair.into_inner();
    let mut statements = Vec::new();
    for stmt in pairs {
        statements.push(parse_statement(stmt)?);
    }
    Ok(statements)
}

fn parse_modifiers(parts: Pairs<Rule>) -> Vec<FunctionModifier> {
    let mut modifiers = Vec::new();
    for inner in parts {
        modifiers.push(match inner.as_str().trim() {
            "extern" => FunctionModifier::Extern,
            "static" => FunctionModifier::Static,
            "public" => FunctionModifier::Public,
            "unsafe" => FunctionModifier::Unsafe,
            "virtual" => FunctionModifier::Virtual,
            "override" => FunctionModifier::Override,
            "abstract" => FunctionModifier::Abstract,
            x => unreachable!("{:?}", x),
        });
    }
    modifiers
}

fn parse_attributes(pairs: &mut Pairs<Rule>) -> Result<Vec<Attribute>> {
    if let Some(peek) = pairs.peek() {
        if peek.as_rule() == Rule::attributes {
            let attributes = pairs.next().unwrap().into_inner();
            let mut results = Vec::new();
            for attribute_statement in attributes {
                let mut attribute_statement = attribute_statement.into_inner();
                let attribute_type = Qualifier::from_idents(attribute_statement.next().unwrap());
                let generic_args = parse_generic_args(&mut attribute_statement)?;
                let mut params = Vec::new();
                for expr in attribute_statement {
                    params.push(parse_expr(expr)?);
                }
                results.push(Attribute {
                    attribute_type,
                    generic_args,
                    params,
                })
            }
            return Ok(results);
        }
    }

    Ok(Vec::new())
}

fn parse_function_decl(mut pairs: Pairs<Rule>, namespace: Option<Qualifier>) -> Result<FunctionDecl> {
    let attributes = parse_attributes(&mut pairs)?;
    let scoped = namespace.is_none();
    let mut modifiers = Vec::new();
    let base_name = loop {
        let tkn = pairs.next().unwrap();
        match tkn.as_rule() {
            Rule::ident => break tkn,
            Rule::function_modifiers => modifiers = parse_modifiers(tkn.into_inner()),
            Rule::keyword_function => (),
            _ => unreachable!(),
        }
    };
    let base_name = Identifier::from_ident(&base_name);
    let name = match namespace {
        Some(qual) => {
            let mut name = qual.into_tokens();
            name.push(base_name);
            name
        }
        None => vec![base_name],
    };
    let generics = parse_generics_decl(&mut pairs)?;
    let mut varargs = Varargs::None;

    let mut params = Vec::new();
    let function_params_decl = pairs.next().unwrap().into_inner();
    for function_param_decl in function_params_decl {
        match function_param_decl.as_rule() {
            Rule::function_param_decl => {
                let mut params_inner = function_param_decl.into_inner();
                let name = Identifier::from_ident(&params_inner.next().unwrap());
                let param_type = QualifiedType::from_idents(params_inner.next().unwrap());
                params.push(NamedParameter {
                    name,
                    param_type,
                });
            }
            Rule::varargs => varargs = Varargs::Array,
            Rule::native_varargs => varargs = Varargs::Native,
            _ => unreachable!(),
        }
    }

    let mut return_type = None;
    let mut body = None;

    if let Some(pair) = pairs.next() {
        match pair.as_rule() {
            Rule::variable_type => {
                return_type = Some(QualifiedType::from_idents(pair));
                if let Some(pair) = pairs.next() {
                    body = Some(parse_block(pair)?);
                }
            }
            Rule::block => body = Some(parse_block(pair)?),
            x => unreachable!("{:?}", x),
        }
    }

    Ok(FunctionDecl {
        attributes,
        function_type: match (scoped, modifiers.contains(&FunctionModifier::Static)) {
            (true, true) => FunctionContextType::Static,
            (true, false) => FunctionContextType::Instance,
            (false, _) => FunctionContextType::Static,
        },
        modifiers,
        name,
        generics,
        params,
        return_type,
        body,
        varargs,
    })
}

fn parse_attribute_decl(mut pairs: Pairs<Rule>, namespace: Qualifier) -> Result<AttributeDecl> {
    for pair in pairs.by_ref() {
        match pair.as_rule() {
            Rule::keyword_attribute => break,
            _ => (),
        }
    }

    let mut name = namespace.into_tokens();
    name.push(Identifier::from_ident(&pairs.next().unwrap()));
    let generics = None;
    let mut params = Vec::new();

    for function_param_decl in pairs {
        let mut params_inner = function_param_decl.into_inner();
        let name = Identifier::from_ident(&params_inner.next().unwrap());
        let param_type = QualifiedType::from_idents(params_inner.next().unwrap());
        params.push(NamedParameter {
            name,
            param_type,
        });
    }

    Ok(AttributeDecl {
        name,
        generics,
        params,
    })
}

fn parse_type_decl(mut pairs: Pairs<Rule>, namespace: Qualifier) -> Result<TypedefDecl> {
    for pair in pairs.by_ref() {
        match pair.as_rule() {
            Rule::keyword_type => break,
            _ => (),
        }
    }

    let mut name = namespace.into_tokens();
    name.push(Identifier::from_ident(&pairs.next().unwrap()));

    let target = QualifiedType::from_idents(pairs.next().unwrap());

    Ok(TypedefDecl {
        name,
        target_type: target,
    })
}

fn parse_generic_decl(mut pairs: Pairs<Rule>) -> GenericDecl {
    let name = Identifier::from_ident(&pairs.next().unwrap());
    let mut interfaces = Vec::new();
    while let Some(interface) = pairs.next().map(Qualifier::from_idents) {
        interfaces.push(interface);
    }
    GenericDecl {
        name,
        interfaces,
    }
}

fn parse_generics_decl(pairs: &mut Pairs<Rule>) -> Result<Option<Vec<GenericDecl>>> {
    if pairs.peek().map(|x| x.as_rule() == Rule::generics_decl).unwrap_or(false) {
        let args = pairs.next().unwrap();
        let types = args.into_inner().map(|arg| parse_generic_decl(arg.into_inner())).collect();
        return Ok(Some(types));
    }

    Ok(None)
}

pub fn parse_generic_args(pairs: &mut Pairs<Rule>) -> Result<Option<GenericArgs>> {
    if pairs.peek().map(|x| x.as_rule() == Rule::generic_args).unwrap_or(false) {
        let args = pairs.next().unwrap();
        let args = args.into_inner().map(QualifiedType::from_idents).collect();
        return Ok(Some(GenericArgs {
            args,
        }));
    }

    Ok(None)
}

fn parse_associated_type(mut pairs: Pairs<Rule>) -> Result<Token<Identifier>> {
    pairs.next(); // skip "type" keyword
    let name = Identifier::from_ident(&pairs.next().unwrap());
    if pairs.peek().is_some() {
        return Err(anyhow!("unexpected type declaration in associated type"));
    }
    Ok(name)
}

fn parse_associated_type_decl(mut pairs: Pairs<Rule>) -> Result<AssociatedTypeDecl> {
    pairs.next(); // skip "type" keyword
    let name = Identifier::from_ident(&pairs.next().unwrap());
    if pairs.peek().is_none() {
        return Err(anyhow!("expecting type declaration in associated type"));
    }
    let ty = QualifiedType::from_idents(pairs.next().unwrap());
    Ok(AssociatedTypeDecl {
        name,
        ty,
    })
}

fn parse_interface_accessor_decl(mut pairs: Pairs<Rule>) -> Result<Vec<AccessorDecl>> {
    let mut accessor_types = Vec::new();
    let name;
    loop {
        let pair = pairs.next().unwrap();
        match pair.as_rule() {
            Rule::keyword_get => {
                accessor_types.push(AccessorType::Getter);
            }
            Rule::keyword_set => {
                accessor_types.push(AccessorType::Setter(Token {
                    token: Identifier("_".to_owned()),
                    loc: TokenLocation {
                        start: pair.as_span().start(),
                        end: pair.as_span().end(),
                    },
                }));
            }
            Rule::ident => {
                name = Identifier::from_ident(&pair);
                break;
            }
            x => unreachable!("{:?}", x),
        }
    }

    let value_type = QualifiedType::from_idents(pairs.next().unwrap());
    Ok(accessor_types
        .into_iter()
        .map(|accessor_type| AccessorDecl {
            modifiers: Vec::new(),
            accessor_type,
            name: name.clone(),
            value_type: value_type.clone(),
            body: None,
        })
        .collect())
}

fn parse_accessor_decl(mut pairs: Pairs<Rule>) -> Result<AccessorDecl> {
    let mut modifiers = Vec::new();
    let accessor_type;
    let name;
    loop {
        let pair = pairs.next().unwrap();
        match pair.as_rule() {
            Rule::keyword_get => {
                name = Identifier::from_ident(&pairs.next().unwrap());
                accessor_type = AccessorType::Getter;
                break;
            }
            Rule::keyword_set => {
                name = Identifier::from_ident(&pairs.next().unwrap());
                accessor_type = AccessorType::Setter(Identifier::from_ident(&pairs.next().unwrap()));
                break;
            }
            Rule::function_modifiers => modifiers = parse_modifiers(pair.into_inner()),
            x => unreachable!("{:?}", x),
        }
    }
    let value_type = QualifiedType::from_idents(pairs.next().unwrap());
    let body = pairs.next().map(parse_block).map_or(Ok(None), |r| r.map(Some))?;

    Ok(AccessorDecl {
        modifiers,
        accessor_type,
        name,
        value_type,
        body,
    })
}

fn parse_interface_block(interface_block: Pairs<Rule>) -> Result<(Vec<FunctionDecl>, Vec<AccessorDecl>, Vec<AssociatedTypeDecl>)> {
    let mut methods = Vec::new();
    let mut accessors = Vec::new();
    let mut associated_types = Vec::new();
    for interface_statement in interface_block {
        match interface_statement.as_rule() {
            Rule::method_decl => methods.push(parse_function_decl(interface_statement.into_inner(), None)?),
            Rule::get_accessor_decl | Rule::set_accessor_decl => accessors.push(parse_accessor_decl(interface_statement.into_inner())?),
            Rule::interface_accessor_decl => accessors.extend(parse_interface_accessor_decl(interface_statement.into_inner())?),
            Rule::associated_type_decl => associated_types.push(parse_associated_type_decl(interface_statement.into_inner())?),
            x => unreachable!("{:?}", x),
        }
    }
    Ok((methods, accessors, associated_types))
}

fn parse_interface_impl(mut pairs: Pairs<Rule>) -> Result<InterfaceImpl> {
    pairs.next(); // skip the "implement" keyword

    let generics = parse_generics_decl(&mut pairs)?;

    let interface_name = Qualifier::from_idents(pairs.next().unwrap());
    let interface_generic_args = parse_generic_args(&mut pairs)?;

    pairs.next(); // skip the "for" keyword

    let target_name = Qualifier::from_idents(pairs.next().unwrap());
    let target_generic_args = parse_generic_args(&mut pairs)?;
    let (functions, accessors, associated_types) = parse_interface_block(pairs.next().unwrap().into_inner())?;

    Ok(InterfaceImpl {
        generics,
        interface_name,
        interface_generic_args,
        target_name,
        target_generic_args,
        functions,
        accessors,
        associated_types,
    })
}

fn parse_anonymous_struct_type_decl(mut pairs: Pairs<Rule>) -> AnonymousStructTypeDecl {
    let mut fields = Vec::new();
    while let Some(name) = pairs.next() {
        let ty = match pairs.peek().map(|pair| pair.as_rule()) {
            Some(Rule::variable_type) => Some(QualifiedType::from_idents(pairs.next().unwrap())),
            _ => None,
        };
        fields.push(AnonymousStructFieldDecl {
            name: Identifier::from_ident(&name),
            ty,
        })
    }

    AnonymousStructTypeDecl {
        fields,
    }
}

fn parse_enum_decl(mut pairs: Pairs<Rule>, namespace: Qualifier) -> Result<EnumDecl> {
    for pair in pairs.by_ref() {
        match pair.as_rule() {
            Rule::keyword_enum => break,
            _ => (),
        }
    }

    let mut name = namespace.into_tokens();
    name.push(Identifier::from_ident(&pairs.next().unwrap()));

    let generics = parse_generics_decl(&mut pairs)?;
    let mut elements = Vec::new();

    let enum_statements = pairs.next().unwrap().into_inner();
    for enum_statement in enum_statements {
        let mut enum_statement = enum_statement.into_inner();
        let name = Identifier::from_ident(&enum_statement.next().unwrap());
        let data = enum_statement.next().map(|anon| parse_anonymous_struct_type_decl(anon.into_inner()));

        elements.push(EnumElementDecl {
            name,
            data,
        })
    }

    Ok(EnumDecl {
        name,
        generics,
        elements,
    })
}

fn parse_class_decl(mut pairs: Pairs<Rule>, namespace: Qualifier, ty: ClassType) -> Result<ClassDecl> {
    for pair in pairs.by_ref() {
        match pair.as_rule() {
            Rule::keyword_class | Rule::keyword_interface | Rule::keyword_struct => break,
            _ => (),
        }
    }
    let mut name = namespace.into_tokens();
    name.push(Identifier::from_ident(&pairs.next().unwrap()));

    let generics = parse_generics_decl(&mut pairs)?;

    let mut fields = Vec::new();
    let mut methods = Vec::new();
    let mut accessors = Vec::new();
    let mut associated_type_names = Vec::new();
    let mut superclass_name = None;
    let mut superclass_generic_args = None;
    let mut constructor = None;
    let mut destructor = None;

    if pairs.peek().unwrap().as_rule() == Rule::class_extends {
        let mut extends = pairs.next().unwrap().into_inner();
        extends.next(); // skip the "extends" keyword

        superclass_name = Some(Qualifier::from_idents(extends.next().unwrap()));
        superclass_generic_args = parse_generic_args(&mut extends)?;
    }

    let class_block = pairs.next().unwrap().into_inner();
    for class_statement in class_block {
        match class_statement.as_rule() {
            Rule::method_decl => methods.push(parse_function_decl(class_statement.into_inner(), None)?),
            Rule::field_decl => fields.push(parse_field_decl(class_statement.into_inner())?),
            Rule::constructor_decl => constructor = Some(parse_block(class_statement)?),
            Rule::destructor_decl => destructor = Some(parse_block(class_statement)?),
            Rule::get_accessor_decl | Rule::set_accessor_decl => accessors.push(parse_accessor_decl(class_statement.into_inner())?),
            Rule::interface_accessor_decl => accessors.extend(parse_interface_accessor_decl(class_statement.into_inner())?),
            Rule::associated_type_decl => associated_type_names.push(parse_associated_type(class_statement.into_inner())?),
            x => unreachable!("{:?}", x),
        }
    }

    Ok(ClassDecl {
        ty,
        name,
        generics,
        superclass_name,
        superclass_generic_args,
        associated_type_names,
        fields,
        methods,
        constructor,
        destructor,
        accessors,
    })
}

fn parse_program<T: AsRef<Path>>(file_path: T, pair: Pair<Rule>) -> Result<KeidFile> {
    let file_path = std::path::absolute(&file_path).unwrap_or_else(|_| file_path.as_ref().to_path_buf());
    let mut pairs = pair.into_inner();

    let mut namespace_statement = pairs.next().unwrap().into_inner();
    namespace_statement.next(); // skip the "namespace" keyword
    let namespace = Qualifier::from_idents(namespace_statement.next().unwrap());

    let mut program = KeidFile {
        source_path: file_path.into_os_string().into_string().unwrap(),
        classes: Vec::new(),
        functions: Vec::new(),
        imports: Vec::new(),
        interface_impls: Vec::new(),
        typedefs: Vec::new(),
        fields: Vec::new(),
        attributes: Vec::new(),
        enums: Vec::new(),
        namespace,
    };

    for pair in pairs {
        match pair.as_rule() {
            Rule::EOI => return Ok(program),
            Rule::import_statement => program.imports.extend(parse_import(pair.into_inner())?),
            Rule::function_decl => program.functions.push(parse_function_decl(pair.into_inner(), Some(program.namespace.clone()))?),
            Rule::class_decl => program.classes.push(parse_class_decl(pair.into_inner(), program.namespace.clone(), ClassType::Class)?),
            Rule::type_decl => {
                program.typedefs.push(parse_type_decl(pair.into_inner(), program.namespace.clone())?);
            }
            Rule::interface_decl => program.classes.push(parse_class_decl(pair.into_inner(), program.namespace.clone(), ClassType::Interface)?),
            Rule::interface_impl => program.interface_impls.push(parse_interface_impl(pair.into_inner())?),
            Rule::let_statement => program.fields.push(parse_let(pair.into_inner())?),
            Rule::struct_decl => program.classes.push(parse_class_decl(pair.into_inner(), program.namespace.clone(), ClassType::Struct)?),
            Rule::attribute_decl => program.attributes.push(parse_attribute_decl(pair.into_inner(), program.namespace.clone())?),
            Rule::enum_decl => program.enums.push(parse_enum_decl(pair.into_inner(), program.namespace.clone())?),
            x => unimplemented!("{:#?}", x),
        }
    }

    unreachable!()
}

pub fn parse_qualified_type(code: &str) -> Result<QualifiedType> {
    let result = SyntaxParser::parse(Rule::variable_type, code);
    match result {
        Ok(mut pairs) => match pairs.next() {
            Some(pair) => Ok(QualifiedType::from_idents(pair)),
            None => Err(anyhow!("no input provided")),
        },
        Err(msg) => Err(msg.into()),
    }
}

pub fn parse_tokens(code: &str) -> Result<Pairs<Rule>> {
    Ok(SyntaxParser::parse(Rule::program, &code)?)
}

pub fn parse(file_name: &str, code: &str) -> Result<KeidFile> {
    let code = preprocessor::preprocess(
        code,
        &PreprocessorContext {
            use_rtdbg: false,
        },
    )?;
    let result = SyntaxParser::parse(Rule::program, &code);
    match result {
        Ok(mut pairs) => match pairs.next() {
            Some(pair) => {
                let program = parse_program(file_name, pair)?;
                Ok(program)
            }
            None => Err(anyhow!("no input provided")),
        },
        Err(msg) => Err(msg.into()),
    }
}
