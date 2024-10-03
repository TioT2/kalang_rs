//! Typed intermediate representation implementation module

use crate::ast::{self, Type};

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

    /// Empty type
    Void,
}

pub enum Expression {
    Constant {
        ty: Box<Type>,
        value: Box<Expression>,
    },
    Array {
        element_ty: Box<Type>,
        values: Vec<Expression>,
    },
}

pub struct Module {}

impl Module {
    pub fn from_ast(_ast: &ast::Module) -> Self {
        todo!()
    }
}

// file mod.rs
