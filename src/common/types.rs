use bincode::{Decode, Encode};

use crate::{
    compiler::{llvm::OpaqueType, Compiler},
    func::utils,
    parser,
    tree::{
        ast::{GenericArgs, Identifier, Token, Varargs},
        ClassType, LookupItem, LookupItemType,
    },
};

use super::{GenericIdentifier, TypeProvider};

pub trait IntoOpaqueType {
    type Output;

    fn as_llvm_type(&self, cpl: &Compiler) -> Self::Output;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Encode, Decode)]
pub struct AnonymousStructField {
    pub name: String,
    pub ty: ComplexType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Encode, Decode)]
pub struct FunctionType {
    pub params: Vec<ComplexType>,
    pub return_type: Box<ComplexType>,
    pub varargs: Varargs,
}

#[macro_export]
macro_rules! primitive_types {
    () => {
        BasicType::Char
            | BasicType::UInt8
            | BasicType::UInt16
            | BasicType::UInt32
            | BasicType::UInt64
            | BasicType::Int8
            | BasicType::Int16
            | BasicType::Int32
            | BasicType::Int64
            | BasicType::USize
            | BasicType::ISize
            | BasicType::Float32
            | BasicType::Float64
            | BasicType::Bool
    };
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Encode, Decode)]
pub enum BasicType {
    Void,
    Bool,
    Char,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Int8,
    Int16,
    Int32,
    Int64,
    Float32,
    Float64,
    USize,
    ISize,
    Function(FunctionType),
    Object(GenericIdentifier),
    AnonymousStruct(Vec<AnonymousStructField>),
    Null,
    /// Used internally by the compiler.
    /// Impossible to achieve syntactically.
    Unknown,
}

impl BasicType {
    pub fn to_complex(self) -> ComplexType {
        ComplexType::Basic(self)
    }

    fn from_type_name(name: &str) -> Option<BasicType> {
        Some(match name {
            "void" => BasicType::Void,
            "bool" => BasicType::Bool,
            "char" => BasicType::Char,
            "uint8" => BasicType::UInt8,
            "uint16" => BasicType::UInt16,
            "uint32" => BasicType::UInt32,
            "uint64" => BasicType::UInt64,
            "int8" => BasicType::Int8,
            "int16" => BasicType::Int16,
            "int32" => BasicType::Int32,
            "int64" => BasicType::Int64,
            "float32" => BasicType::Float32,
            "float64" => BasicType::Float64,
            "usize" => BasicType::USize,
            "isize" => BasicType::ISize,
            _ => return None,
        })
    }

    pub fn from_ast(tokens: &[Token<Identifier>], generic_args: Option<GenericArgs>) -> BasicType {
        if let Some(basic) = Self::from_type_name(tokens[0].token.0.as_str()) {
            basic
        } else {
            let full_name = tokens.iter().map(|part| part.token.0.as_str()).collect::<Vec<&str>>().join("::");
            BasicType::Object(GenericIdentifier::from_name_with_args(
                &full_name,
                &generic_args
                    .as_ref()
                    .map(|args| args.args.iter().map(|qual| qual.complex.clone()).collect::<Vec<_>>())
                    .unwrap_or_default(),
            ))
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            BasicType::Int8 | BasicType::Int16 | BasicType::Int32 | BasicType::Int64 => true,
            _ => false,
        }
    }
}

impl IntoOpaqueType for BasicType {
    type Output = OpaqueType;

    fn as_llvm_type(&self, cpl: &Compiler) -> OpaqueType {
        match self {
            BasicType::Void => cpl.context.get_void_type(),
            BasicType::Bool => cpl.context.get_i1_type(),
            BasicType::Int8 => cpl.context.get_i8_type(),
            BasicType::Char => cpl.context.get_i32_type(),
            BasicType::Int16 => cpl.context.get_i16_type(),
            BasicType::Int32 => cpl.context.get_i32_type(),
            BasicType::Int64 => cpl.context.get_i64_type(),
            BasicType::UInt8 => cpl.context.get_i8_type(),
            BasicType::UInt16 => cpl.context.get_i16_type(),
            BasicType::UInt32 => cpl.context.get_i32_type(),
            BasicType::UInt64 => cpl.context.get_i64_type(),
            BasicType::Float32 => cpl.context.get_f32_type(),
            BasicType::Float64 => cpl.context.get_f64_type(),
            BasicType::Null => panic!("null has no LLVM type"),
            BasicType::Unknown => cpl.context.get_pointer_type(cpl.context.get_void_type()),
            BasicType::ISize | BasicType::USize => cpl.context.get_isize_type(),
            BasicType::Object(identifier) => match cpl.type_provider.get_class_by_name(identifier) {
                Some(class_impl) => match class_impl.class_type {
                    ClassType::Class | ClassType::Interface => {
                        cpl.context.get_pointer_type(cpl.context.get_abi_class_data_type(cpl, &class_impl))
                    }
                    ClassType::Struct => cpl.context.get_abi_class_data_type(cpl, &class_impl),
                    ClassType::Enum => unreachable!(),
                },
                None => match cpl.type_provider.get_enum_by_name(identifier) {
                    Some(enum_impl) => cpl.context.get_abi_enum_type_any_element(cpl, &enum_impl),
                    None => {
                        if let Some(generic) = cpl.type_provider.context_generics.get(&self.clone().to_complex()) {
                            generic.as_llvm_type(cpl)
                        } else {
                            panic!("unresolved type: `{}`", identifier.to_string());
                        }
                    }
                },
            },
            BasicType::AnonymousStruct(members) => {
                let name =
                    members.iter().map(|member| format!("{}:{}", member.name, member.ty.to_string())).collect::<Vec<String>>().join(",");
                let name = format!("AnonymousStruct#{}", name);
                cpl.context.get_struct_type(&name, &members.iter().map(|member| member.ty.as_llvm_type(cpl)).collect::<Vec<OpaqueType>>())
            }
            BasicType::Function(_) => cpl.context.get_pointer_type(cpl.context.get_abi_any_closure_type()),
        }
    }
}

impl ToString for BasicType {
    fn to_string(&self) -> String {
        String::from(match self {
            BasicType::Void => "void",
            BasicType::Bool => "bool",
            BasicType::Char => "char",
            BasicType::UInt8 => "uint8",
            BasicType::UInt16 => "uint16",
            BasicType::UInt32 => "uint32",
            BasicType::UInt64 => "uint64",
            BasicType::Int8 => "int8",
            BasicType::Int16 => "int16",
            BasicType::Int32 => "int32",
            BasicType::Int64 => "int64",
            BasicType::Float32 => "float32",
            BasicType::Float64 => "float64",
            BasicType::USize => "usize",
            BasicType::ISize => "isize",
            BasicType::Object(identifier) => return identifier.to_string(),
            BasicType::AnonymousStruct(members) => {
                return format!(
                    "{{ {}, }}",
                    members.iter().map(|member| format!("{}: {}", member.name, member.ty.to_string())).collect::<Vec<String>>().join(", ")
                )
            }
            BasicType::Null => "null",
            BasicType::Unknown => "{unknown}",
            BasicType::Function(ft) => {
                return format!("function({})", utils::iter_join(&ft.params.iter().map(|param| param.to_string()).collect::<Vec<String>>()))
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Encode, Decode)]
pub enum ComplexType {
    Basic(BasicType),
    Reference(Box<ComplexType>),
    Array(Box<ComplexType>),
    Nullable(Box<ComplexType>),
    Spread(Box<ComplexType>),
}

impl ComplexType {
    pub fn is_struct(&self, type_provider: &TypeProvider) -> bool {
        match &self {
            ComplexType::Nullable(_) | ComplexType::Array(_) => true,
            ComplexType::Basic(BasicType::Object(ident)) => match type_provider.get_class_by_name(ident) {
                Some(class_impl) => class_impl.class_type == ClassType::Struct,
                None => type_provider.get_enum_by_name(ident).is_some(),
            },
            _ => false,
        }
    }

    pub fn is_struct_with(&self, types: &[LookupItem]) -> bool {
        match &self {
            ComplexType::Nullable(_) | ComplexType::Array(_) => true,
            ComplexType::Basic(BasicType::Object(ident)) => {
                types.iter().any(|ty| ty.name == ident.name && matches!(ty.ty, LookupItemType::Struct | LookupItemType::Enum))
            }
            _ => false,
        }
    }

    pub fn replace_root(self, new_root: ComplexType) -> ComplexType {
        match self {
            ComplexType::Basic(_) => new_root,
            ComplexType::Reference(reference) => ComplexType::Reference(Box::new(reference.replace_root(new_root))),
            ComplexType::Array(array) => ComplexType::Array(Box::new(array.replace_root(new_root))),
            ComplexType::Nullable(nullable) => ComplexType::Nullable(Box::new(nullable.replace_root(new_root))),
            ComplexType::Spread(spread) => ComplexType::Spread(Box::new(spread.replace_root(new_root))),
        }
    }

    pub fn try_dereference(&self) -> Option<&ComplexType> {
        match self {
            Self::Reference(complex) => Some(complex),
            _ => None,
        }
    }

    pub fn try_element_type(&self) -> Option<&ComplexType> {
        match self {
            Self::Array(element) => Some(element),
            _ => None,
        }
    }

    pub fn try_nullable_type(&self) -> Option<&ComplexType> {
        match self {
            Self::Nullable(element) => Some(element),
            _ => None,
        }
    }

    pub fn get_root_type(&self) -> BasicType {
        match self {
            Self::Basic(ty) => ty.clone(),
            Self::Reference(inner) | Self::Nullable(inner) | Self::Array(inner) | Self::Spread(inner) => inner.get_root_type(),
        }
    }

    pub fn to_reference(self) -> ComplexType {
        ComplexType::Reference(Box::new(self))
    }

    pub fn to_array(self) -> ComplexType {
        ComplexType::Array(Box::new(self))
    }
}

impl IntoOpaqueType for ComplexType {
    type Output = OpaqueType;

    fn as_llvm_type(&self, cpl: &Compiler) -> Self::Output {
        match self {
            Self::Basic(basic) => basic.as_llvm_type(cpl),
            Self::Reference(complex) => cpl.context.get_pointer_type(complex.as_llvm_type(cpl)),
            Self::Nullable(item) => cpl.context.get_abi_nullable_type(item.as_llvm_type(cpl), &item.to_string()),
            Self::Array(element) => cpl.context.get_abi_slice_type(element.as_llvm_type(cpl), &element.to_string()),
            Self::Spread(_) => panic!("spreads and only exist at Keid compile time (pre-LLVM)"),
        }
    }
}

impl ToString for ComplexType {
    fn to_string(&self) -> String {
        match self {
            Self::Basic(basic) => basic.to_string(),
            Self::Reference(inner) => format!("ptr~{}", inner.to_string()),
            Self::Array(array) => format!("[{}]", array.to_string()),
            Self::Nullable(nullable) => format!("?{}", nullable.to_string()),
            Self::Spread(spread) => format!("...{}", spread.to_string()),
        }
    }
}

impl From<&str> for ComplexType {
    fn from(value: &str) -> Self {
        parser::parse_qualified_type(value).expect("invalid complex type").complex
    }
}
