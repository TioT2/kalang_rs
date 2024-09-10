use comb::{PResult, Parser};

use crate::lexer::{Literal, Symbol, Token};

use super::{PrimitiveType, Type};

/// Some symbol parser getting function
pub fn symbol<'t>(match_sm: Symbol) -> impl Parser<'t, &'t [Token<'t>], ()> {
    move |tl: &'t [Token]| -> PResult<'t, &'t [Token], ()> {
        if let Some(Token::Symbol(sm)) = tl.get(0) {
            if *sm == match_sm {
                return Ok((&tl[1..], ()));
            }
        }

        return Err(tl);
    }
} // fn symbol

/// Any ident parser
pub fn ident<'t>(tl: &'t [Token]) -> PResult<'t, &'t [Token<'t>], &'t str> {
    if let Some(Token::Ident(str)) = tl.get(0) {
        Ok((&tl[1..], str))
    } else {
        Err(tl)
    }
} // fn ident

/// Any literal parser
 pub fn literal<'t>(tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], Literal> {
    if let Some(Token::Literal(lit)) = tl.get(0) {
        Ok((&tl[1..], *lit))
    } else {
        Err(tl)
    }
} // fn literal

/// INT literal parsing function
pub fn integer_literal<'t>(tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], u64> {
    if let Some(Token::Literal(Literal::Integer(int))) = tl.get(0) {
        Ok((&tl[1..], *int))
    } else {
        Err(tl)
    }
} // fn integer_literal

pub fn ty<'t>(tl: &'t [Token<'t>]) -> PResult<'t, &'t [Token<'t>], Type> {
    let token = tl.get(0).ok_or(tl)?;

    return match token {
        // function pointer
        Token::Symbol(Symbol::Fn) => {
            comb::map(
                comb::all((
                    symbol(Symbol::RoundBrOpen),
                    comb::repeat_with_separator(
                        ty,
                        symbol(Symbol::Comma),
                        Vec::new,
                        |mut vec, ty| {
                            vec.push(ty);
                            vec
                        }
                    ),
                    symbol(Symbol::RoundBrClose),
                    comb::map(ty, Box::new),
                )),
                |(_, inputs, _, output)| Type::FunctionPointer {
                    inputs,
                    output,
                }
            )(&tl[1..])
        }
        // tuple
        Token::Symbol(Symbol::RoundBrOpen) => {
            comb::map(
                comb::all((
                    comb::repeat_with_separator(
                        ty,
                        symbol(Symbol::Comma),
                        Vec::new,
                        |mut vec, ty| {
                            vec.push(ty);
                            vec
                        }
                    ),
                    symbol(Symbol::RoundBrClose),
                )),
                |(element_types, _)| Type::Tuple { element_types }
            )(&tl[1..])
        }
        // array
        Token::Symbol(Symbol::SquareBrOpen) => {
            comb::map(
                comb::all((
                    comb::map(ty, Box::new),
                    symbol(Symbol::Semicolon),
                    comb::filter_map(
                        literal,
                        |literal| match literal {
                            Literal::Integer(i) => Some(i as usize),
                            _ => None
                        }
                    ),
                    symbol(Symbol::SquareBrClose),
                )),
                |(element_type, _, size, _)| Type::Array {
                    element_type,
                    size
                }
            )(&tl[1..])
        }
        Token::Symbol(Symbol::Asterisk) => {
            comb::map(
                comb::all((
                    comb::any((
                        comb::map(symbol(Symbol::Mut  ), |_| true ),
                        comb::map(symbol(Symbol::Const), |_| false),
                        comb::map(comb::identity,        |_| false),
                    )),
                    comb::map(ty, Box::new),
                )),
                |(is_mutable, element_type)| Type::Pointer { element_type, is_mutable }
            )(&tl[1..])
        }
        Token::Ident(ident) => {
            // try parse primitive
            if let Some(primitive) = PrimitiveType::parse(ident) {
                Ok((&tl[1..], Type::Primitive(primitive)))
            } else {
                Ok((&tl[1..], Type::Named(ident.to_string())))
            }
        }
        _ => Err(tl),
    };
}

