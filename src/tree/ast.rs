use super::ClassType;
use crate::common::types::{AnonymousStructField, FunctionType};
use crate::parser::Rule;
use crate::{
    common::types::{BasicType, ComplexType},
    parser,
};
use bincode::{Decode, Encode};
use pest::iterators::{Pair, Pairs};

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn from_ident(ident: &Pair<Rule>) -> Token<Identifier> {
        Token {
            token: Identifier(ident.as_str().to_owned()),
            loc: parser::span_to_location(&ident.as_span()),
        }
    }

    pub fn from_string(string: &str) -> Token<Identifier> {
        Token {
            token: Identifier(string.to_owned()),
            loc: TokenLocation {
                start: 0,
                end: 0,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Qualifier(pub Vec<Token<Identifier>>);

impl Qualifier {
    pub fn into_tokens(self) -> Vec<Token<Identifier>> {
        self.0
    }

    pub fn from_idents(idents: Pair<Rule>) -> Qualifier {
        let mut path = Vec::new();

        let tokens = idents.into_inner().flatten();
        for token in tokens {
            if token.as_rule() == Rule::ident {
                path.push(Identifier::from_ident(&token));
            }
        }

        Qualifier(path)
    }

    pub fn get_location(&self) -> TokenLocation {
        if self.0.is_empty() {
            panic!("empty qualifier has no location");
        }
        TokenLocation {
            start: self.0[0].loc.start,
            end: self.0[self.0.len() - 1].loc.end,
        }
    }
}

impl ToString for Qualifier {
    fn to_string(&self) -> String {
        self.0.iter().map(|part| &part.token.0).fold(String::new(), |a, b| a + b + "::").trim_end_matches(':').to_owned()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct QualifiedType {
    pub complex: ComplexType,
    pub loc: TokenLocation,
}

impl QualifiedType {
    pub fn from_qualifier(qual: &Qualifier) -> QualifiedType {
        QualifiedType {
            complex: ComplexType::Basic(BasicType::from_ast(&qual.0, None)),
            loc: qual.get_location(),
        }
    }

    fn parse_next(mut pairs: Pairs<Rule>) -> ComplexType {
        let token = pairs.peek().unwrap();
        match token.as_rule() {
            Rule::nullable => {
                pairs.next();
                ComplexType::Nullable(Box::new(Self::parse_next(pairs)))
            }
            Rule::array_type => {
                pairs.next();
                ComplexType::Array(Box::new(Self::from_idents(token.into_inner().next().unwrap()).complex))
            }
            Rule::anonymous_struct_type => {
                pairs.next();
                let mut inner = token.into_inner();

                let mut members = Vec::new();
                while let (Some(name), Some(ty)) = (inner.next(), inner.next()) {
                    members.push(AnonymousStructField {
                        name: Identifier::from_ident(&name).token.0,
                        ty: QualifiedType::from_idents(ty).complex,
                    })
                }

                ComplexType::Basic(BasicType::AnonymousStruct(members))
            }
            Rule::function_type => {
                pairs.next();
                let mut inner = token.into_inner();
                let function_params_decl = inner.next().unwrap().into_inner();
                let mut varargs = Varargs::None;
                let mut params = Vec::new();
                for function_param_decl in function_params_decl {
                    match function_param_decl.as_rule() {
                        Rule::function_param_decl => {
                            let mut params_inner = function_param_decl.into_inner();
                            params_inner.next(); // skip the name
                            let param_type = QualifiedType::from_idents(params_inner.next().unwrap());
                            params.push(param_type.complex);
                        }
                        Rule::varargs => varargs = Varargs::Array,
                        Rule::native_varargs => varargs = Varargs::Native,
                        _ => unreachable!(),
                    }
                }
                let return_type = QualifiedType::from_idents(inner.next().unwrap());
                ComplexType::Basic(BasicType::Function(FunctionType {
                    params,
                    varargs,
                    return_type: Box::new(return_type.complex),
                }))
            }
            _ => {
                let mut path = Vec::new();
                let mut generic_args = None;

                for token in pairs {
                    match token.as_rule() {
                        Rule::qualifier => path.extend(Qualifier::from_idents(token).0),
                        Rule::ident => path.push(Identifier::from_ident(&token)),
                        Rule::generic_args => {
                            let args = token.into_inner().map(QualifiedType::from_idents).collect();
                            generic_args = Some(GenericArgs {
                                args,
                            })
                        }
                        _ => unreachable!(),
                    }
                }

                ComplexType::Basic(BasicType::from_ast(&path, generic_args))
            }
        }
    }

    pub fn from_idents(idents: Pair<Rule>) -> QualifiedType {
        QualifiedType {
            loc: TokenLocation {
                start: idents.as_span().start(),
                end: idents.as_span().end(),
            },
            complex: { Self::parse_next(idents.into_inner()) },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeidFile {
    pub source_path: String,
    pub namespace: Qualifier,
    pub imports: Vec<Qualifier>,
    pub classes: Vec<ClassDecl>,
    pub functions: Vec<FunctionDecl>,
    pub interface_impls: Vec<InterfaceImpl>,
    pub typedefs: Vec<TypedefDecl>,
    pub fields: Vec<Let>,
    pub attributes: Vec<AttributeDecl>,
    pub enums: Vec<EnumDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedParameter {
    pub name: Token<Identifier>,
    pub param_type: QualifiedType,
}

#[derive(Debug, Clone, PartialEq, Encode, Decode)]
pub enum FunctionModifier {
    Extern,
    Static,
    Public,
    Internal,
    Unsafe,
    Virtual,
    Override,
    Abstract,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericDecl {
    pub name: Token<Identifier>,
    pub interfaces: Vec<Qualifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericArgs {
    pub args: Vec<QualifiedType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AttributeDecl {
    pub name: Vec<Token<Identifier>>,
    pub generics: Option<Vec<GenericDecl>>,
    pub params: Vec<NamedParameter>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    pub ty: ClassType,
    pub name: Vec<Token<Identifier>>,
    pub generics: Option<Vec<GenericDecl>>,
    pub superclass_name: Option<Qualifier>,
    pub superclass_generic_args: Option<GenericArgs>,
    pub associated_type_names: Vec<Token<Identifier>>,
    pub fields: Vec<Let>,
    pub methods: Vec<FunctionDecl>,
    pub constructor: Option<Vec<Token<Statement>>>,
    pub destructor: Option<Vec<Token<Statement>>>,
    pub accessors: Vec<AccessorDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnonymousStructFieldDecl {
    pub name: Token<Identifier>,
    pub ty: Option<QualifiedType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnonymousStructTypeDecl {
    pub fields: Vec<AnonymousStructFieldDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumElementDecl {
    pub name: Token<Identifier>,
    pub data: Option<AnonymousStructTypeDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub name: Vec<Token<Identifier>>,
    pub generics: Option<Vec<GenericDecl>>,
    pub elements: Vec<EnumElementDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedefDecl {
    pub name: Vec<Token<Identifier>>,
    pub target_type: QualifiedType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssociatedTypeDecl {
    pub name: Token<Identifier>,
    pub ty: QualifiedType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceImpl {
    pub generics: Option<Vec<GenericDecl>>,
    pub associated_types: Vec<AssociatedTypeDecl>,
    pub interface_name: Qualifier,
    pub interface_generic_args: Option<GenericArgs>,
    pub target_name: Qualifier,
    pub target_generic_args: Option<GenericArgs>,
    pub functions: Vec<FunctionDecl>,
    pub accessors: Vec<AccessorDecl>,
}

#[derive(Debug, Clone, PartialEq, Encode, Decode)]
pub enum FunctionContextType {
    Instance,
    Static,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub attribute_type: Qualifier,
    pub generic_args: Option<GenericArgs>,
    pub params: Vec<Token<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub attributes: Vec<Attribute>,
    pub function_type: FunctionContextType,
    pub modifiers: Vec<FunctionModifier>,
    pub name: Vec<Token<Identifier>>,
    pub generics: Option<Vec<GenericDecl>>,
    pub params: Vec<NamedParameter>,
    pub return_type: Option<QualifiedType>,
    pub body: Option<Vec<Token<Statement>>>,
    pub varargs: Varargs,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encode, Decode)]
pub enum Varargs {
    None,
    Array,
    Native,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AccessorDecl {
    pub modifiers: Vec<FunctionModifier>,
    pub accessor_type: AccessorType,
    pub name: Token<Identifier>,
    pub value_type: QualifiedType,
    pub body: Option<Vec<Token<Statement>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessorType {
    Getter,
    Setter(Token<Identifier>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub deref: bool,
    pub lhs: Token<Expr>,
    pub op: Operator,
    pub rhs: Token<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding {
    pub name: Token<Identifier>,
    pub var_type: Option<QualifiedType>,
    pub initial_value: Option<Token<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub is_extern: bool,
    pub is_const: bool,
    pub bindings: Vec<LetBinding>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub callee: Box<Token<Expr>>,
    pub generic_args: Option<GenericArgs>,
    pub args: Vec<Token<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NewCallField {
    pub field_name: Token<Identifier>,
    pub value: Option<Token<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NewCall {
    pub ty: QualifiedType,
    pub args: Vec<NewCallField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NewArray {
    pub element_type: QualifiedType,
    pub initial_value: Box<Token<Expr>>,
    pub length: Box<Token<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpecifiedArray {
    pub element_type: QualifiedType,
    pub initial_values: Vec<Token<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticFuncCall {
    pub owner: Qualifier,
    pub call: FuncCall,
}

impl StaticFuncCall {
    pub fn get_full_name(&self) -> String {
        let mut full_parts = self.owner.clone().into_tokens();
        full_parts.push(match self.call.callee.token.clone() {
            Expr::Ident(ident) => ident,
            _ => unreachable!(),
        });
        full_parts.iter().map(|part| &part.token.0).fold(String::new(), |a, b| a + b + "::").trim_end_matches(':').to_owned()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalBlock {
    pub test: Box<Token<Expr>>,
    pub body: Vec<Token<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfChain {
    /// Contains the `if` / `if else` blocks in ordxer.
    pub conditionals: Vec<ConditionalBlock>,
    /// Contains the `else` block if it exists
    pub fallback: Option<Vec<Token<Statement>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForLoop {
    pub variable: Token<Identifier>,
    pub iterator: Token<Expr>,
    pub block: Vec<Token<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoop {
    pub condition: Token<Expr>,
    pub block: Vec<Token<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FixedBlock {
    pub variable: Let,
    pub block: Vec<Token<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TryCatch {
    pub try_block: Vec<Token<Statement>>,
    pub error_var: Option<Token<Identifier>>,
    pub catch_block: Vec<Token<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(Let),
    Return(Option<Token<Expr>>),
    Assign(Assign),
    IfChain(IfChain),
    ForLoop(ForLoop),
    WhileLoop(WhileLoop),
    Expr(Token<Expr>),
    ArrowExpr(Token<Expr>),
    Block(Vec<Token<Statement>>),
    UnsafeBlock(Vec<Token<Statement>>),
    FixedBlock(FixedBlock),
    IndefiniteLoop(Vec<Token<Statement>>),
    Unreachable,
    Throw(Token<Expr>),
    TryCatch(TryCatch),
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicExpr {
    pub lhs: Box<Token<Expr>>,
    pub rhs: Box<Token<Expr>>,
    pub op: Operator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: Operator,
    pub value: Box<Token<Expr>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Equals,
    NotEquals,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    LeftShift,
    RightShift,
    BooleanOr,
    BooleanAnd,
    LessThanOrEquals,
    GreaterThanOrEquals,
    LessThan,
    GreaterThan,
    As,
    Not,
    NonNullAssertion,
    Spread,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cast {
    value: Box<Expr>,
    target_type: QualifiedType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub start: Box<Token<Expr>>,
    pub end: Box<Token<Expr>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MemberType {
    Root,
    Class,
    Array,
    Slice,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub ty: MemberType,
    pub value: Token<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpr {
    pub prefix: Option<Qualifier>,
    pub members: Vec<Member>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumWithDataExpr {
    pub declaring_type: Qualifier,
    pub member: Token<Identifier>,
    pub data: Vec<NewCallField>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchExprBranchArg {
    Catchall(TokenLocation),
    Enum(Token<Identifier>),
    EnumWithData {
        member: Token<Identifier>,
        data_type: AnonymousStructTypeDecl,
    },
    Expr(Token<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExprBranch {
    pub arg: MatchExprBranchArg,
    pub statement: Token<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
    pub value: Box<Token<Expr>>,
    pub branches: Vec<MatchExprBranch>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SubsliceExpr {
    pub start: Option<Box<Token<Expr>>>,
    pub end: Option<Box<Token<Expr>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Null,
    BoolLit(bool),
    StringLit(String),
    CharLit(char),
    SignedIntLit(i64),
    EnumWithData(EnumWithDataExpr),
    FuncCall(FuncCall),
    Ident(Token<Identifier>),
    Member(MemberExpr),
    New(NewCall),
    NewArray(NewArray),
    SpecifiedArray(SpecifiedArray),
    Reference(Box<Token<Expr>>),
    Dereference(Box<Token<Expr>>),
    Logic(LogicExpr),
    Unary(UnaryExpr),
    CastTarget(QualifiedType),
    Range(Range),
    Default(QualifiedType),
    SizeOf(QualifiedType),
    AnonymousStruct(Vec<NewCallField>),
    Match(MatchExpr),
    Subslice(SubsliceExpr),
    Psuedo(crate::common::TypedValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenLocation {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<T: Clone + PartialEq> {
    pub token: T,
    pub loc: TokenLocation,
}
