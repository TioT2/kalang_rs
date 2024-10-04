//! Typed intermediate representation implementation module

use std::collections::HashMap;

use crate::ast::{self, PrimitiveType, Mutability};

/// Type representation structure
#[derive(Debug, Clone)]
pub enum Type {
    /// Primitive
    Primitive(PrimitiveType),

    /// Structure identifier
    Structure(String),

    /// Enumeration identifier
    Enumeration(String),

    /// Array
    Array {
        /// Single element type
        element_type: Box<Type>,

        /// Array size
        size: usize,
    },

    /// Pointer
    Pointer {
        /// Element type
        element_type: Box<Type>,

        /// Element mutability
        mutability: Mutability,
    },

    /// Tuple
    Tuple {
        /// Set of element types
        element_types: Vec<Type>,
    },

    /// Pointer to function
    FunctionPointer {
        /// Set of function pointer inputs
        inputs: Vec<Type>,

        /// Function output value
        output: Box<Type>,
    },
} // enum Type

/// Constant of primitive type
pub enum PrimitiveConstant {
    /// 8 bit signed integer
    I8(i8),

    /// 8 bit unsigned integer
    U8(u8),

    /// 16 bit signed integer
    I16(i16),

    /// 16 bit unsigned integer
    U16(u16),

    /// 32 bit signed integer
    I32(i32),

    /// 32 bit unsigned integer
    U32(u32),

    /// 32 bit FP number
    F32(f32),

    /// 64 bit signed integer
    I64(i64),

    /// 64 bit unsigned integer
    U64(u64),

    /// 64 bit FP number
    F64(f64),

    /// Unsigned number with size of pointer
    Usize(usize),

    /// Signed number with size of pointer
    Isize(isize),

    /// Character type
    Char(char),
} // enum PrimitiveConstant

/// Expression representation enumeration
pub struct Expression {
}

/// Lowered enumeraion representaiton enumeration
pub struct Enumeration {
    /// Enumeration variants
    pub variants: HashMap<String, usize>,
} // struct Enumeration

/// Lowered structure representation structure
pub struct Structure {
    /// Structure fields
    pub fields: HashMap<String, Type>,
} // struct Structure

/// AST Lowering error
#[derive(Clone, Debug)]
pub enum AstLoweringError {
    /// Duplicate enumeration names
    DuplicateEnumerationElementNames {
        /// Enumeration name
        enum_name: String,

        /// Element name
        name: String,

        /// First element index
        first: usize,

        /// Second element index
        second: usize,
    },

    /// Duplicate structure field names
    DuplicateStructureFieldNames {
        /// Structure name
        struct_name: String,

        /// Field names
        name: String,

        /// First field type
        first_ty: Type,

        /// Second field type
        second_ty: Type,
    },

    /// Duplicate enumerations
    DuplicateEnumerations {
        /// Enumeration name
        name: String,
    },

    /// Duplicate structure occurrence
    DuplicateStructures {
        /// Structure name
        name: String,
    },

    /// Undefined type symbol
    UndefinedTypeSymbol {
        name: String,
    },

    /// Actually valid, but not yet implemented language feature
    UnimplementedFeature(UnimplementedFeature),
} // enum AstLoweringError

pub struct ModuleBuilder {
    enumerations: HashMap<String, Enumeration>,
    structures: HashMap<String, Structure>,
}

impl ModuleBuilder {
    pub fn new() -> Self {
        Self {
            enumerations: HashMap::new(),
            structures: HashMap::new(),
        }
    }

    pub fn lower_ty(&self, ty: &ast::Type) -> Result<Type, AstLoweringError> {
        match ty {
            ast::Type::Array { element_type, size } => {
                Ok(Type::Array {
                    element_type: Box::new(self.lower_ty(&element_type.as_ref())?),
                    size: *size,
                })
            }
            ast::Type::FunctionPointer { inputs, output } => {
                Ok(Type::FunctionPointer {
                    inputs: inputs
                        .iter()
                        .map(|ty| self.lower_ty(ty))
                        .collect::<Result<Vec<Type>, AstLoweringError>>()
                        ?,
                    output: Box::new(self.lower_ty(output.as_ref())?),
                })
            }
            ast::Type::Named(str) => {
                if self.enumerations.contains_key(str) {
                    Ok(Type::Enumeration(str.clone()))
                } else if self.structures.contains_key(str) {
                    Ok(Type::Structure(str.clone()))
                } else {
                    Err(AstLoweringError::UndefinedTypeSymbol {
                        name: str.clone(),
                    })
                }
            }
            ast::Type::Primitive(prim) => {
                Ok(Type::Primitive(*prim))
            }
            ast::Type::Pointer { element_type, mutability } => {
                Ok(Type::Pointer {
                    element_type: Box::new(self.lower_ty(element_type.as_ref())?),
                    mutability: *mutability,
                })
            }
            ast::Type::Tuple { element_types } => {
                Ok(Type::Tuple {
                    element_types: element_types
                        .iter()
                        .map(|v| {
                            self.lower_ty(v)
                        })
                        .collect::<Result<Vec<Type>, AstLoweringError>>()
                        ?,
                })
            }
        }
    }

    pub fn lower_structure(&mut self, name: &str, structure: &ast::Structure) -> Result<(), AstLoweringError> {
        let mut result = Structure {
            fields: HashMap::new(),
        };

        for (field_name, field_ty) in structure.elements.iter() {
            let lowered_field_ty = self.lower_ty(field_ty)?;
            let same_named_field = result.fields.insert(field_name.clone(), lowered_field_ty.clone());

            if let Some(ty) = same_named_field {
                return Err(AstLoweringError::DuplicateStructureFieldNames {
                    struct_name: name.to_string(),
                    name: field_name.clone(),
                    first_ty: ty,
                    second_ty: lowered_field_ty,
                });
            }
        }

        if self.structures.insert(name.to_string(), result).is_some() {
            return Err(AstLoweringError::DuplicateEnumerations {
                name: name.to_string()
            });
        }

        Ok(())
    }

    pub fn lower_enumeration(&mut self, name: &str, enumeration: &ast::Enumeration) -> Result<(), AstLoweringError> {
        let mut result = Enumeration {
            variants: HashMap::new()
        };

        for (index, (option_name, initializer)) in enumeration.variants.iter().enumerate() {
            if !initializer.is_none() {
                return Err(UnimplementedFeature::ExplicitEnumValue.into());
            }

            if let Some(duplicate_index) = result.variants.insert(option_name.clone(), index) {
                return Err(AstLoweringError::DuplicateEnumerationElementNames {
                    enum_name: name.to_string(),
                    name: option_name.clone(),
                    first: index,
                    second: duplicate_index,
                });
            }
        }

        if self.enumerations.insert(name.to_string(), result).is_some() {
            return Err(AstLoweringError::DuplicateEnumerations {
                name: name.to_string()
            });
        }

        Ok(())
    }
}

pub struct Module {

} //struct Module

/// Unimplemented language feature
#[derive(Clone, Debug)]
pub enum UnimplementedFeature {
    /// Explicit enumeration value setting (yeah)
    ExplicitEnumValue,
} // enum UnimplementedFeature

impl From<UnimplementedFeature> for AstLoweringError {
    fn from(value: UnimplementedFeature) -> Self {
        Self::UnimplementedFeature(value)
    }
}

impl Module {
    pub fn lower_ast(ast: &ast::Module) -> Result<Self, AstLoweringError> {

        let mut builder = ModuleBuilder::new();
        for (name, declaration) in ast.declarations.iter() {
            match declaration {
                ast::Declaration::Enumeration(enu) => {
                    builder.lower_enumeration(name, enu)?;
                },
                ast::Declaration::Structure(str) => {
                    builder.lower_structure(name, str)?;
                },

                ast::Declaration::Function(_fun) => todo!(),
                ast::Declaration::Variable(_var) => todo!(),

            }
        }
        todo!()
    }
}

// file mod.rs
