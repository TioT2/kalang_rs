//! Typed intermediate representation implementation module

use crate::ast;

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

pub struct Enum {
    /// Enumeration variants
    pub variants: Vec<(String, usize)>,
} // struct Enum

pub struct Module {

} //struct Module

/// Unimplemented language feature
#[derive(Clone, Debug)]
pub enum UnimplementedFeature {
    /// Explicit enumeration value setting (yeah)
    ExplicitEnumValue,
}

#[derive(Clone, Debug)]
pub enum AstLoweringError {
    UnimplementedFeature(UnimplementedFeature),
}

impl Module {
    pub fn lower_ast(_ast: &ast::Module) -> Result<Self, AstLoweringError> {
        todo!()
    }
}

// file mod.rs
