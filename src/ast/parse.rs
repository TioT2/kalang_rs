use comb::{PResult, Parser};

use crate::lexer::{Keyword, Literal, Symbol, Token};

use super::{Operator, Type};

pub fn keyword<'t>(match_kw: Keyword) -> impl Parser<'t, &'t [Token], ()> {
    move |tl: &'t [Token]| -> PResult<'t, &'t [Token], ()> {
        if let Some(Token::Keyword(kw)) = tl.get(0) {
            if *kw == match_kw {
                return Ok((&tl[1..], ()))
            }
        }

        return Err(tl);
    }
}

pub fn symbol<'t>(match_sm: Symbol) -> impl Parser<'t, &'t [Token], ()> {
    move |tl: &'t [Token]| -> PResult<'t, &'t [Token], ()> {
        if let Some(Token::Symbol(sm)) = tl.get(0) {
            if *sm == match_sm {
                return Ok((&tl[1..], ()));
            }
        }

        return Err(tl);
    }
}

pub fn operator(tl: &[Token]) -> PResult<&[Token], Operator> {
    let Some(Token::Symbol(symbol)) = tl.get(0) else {
        return Err(tl);
    };
    let symbol = *symbol;

    let oper = match symbol {
        Symbol::Plus     => Operator::Add,
        Symbol::Minus    => Operator::Sub,
        Symbol::Asterisk => Operator::Mul,
        Symbol::Slash    => Operator::Div,

        Symbol::PlusEqual     => Operator::AddAssign,
        Symbol::MinusEqual    => Operator::SubAssign,
        Symbol::AsteriskEqual => Operator::MulAssign,
        Symbol::SlashEqual    => Operator::DivAssign,

        _ => return Err(tl),
    };

    Ok((&tl[1..], oper))
}

pub fn ident(tl: &[Token]) -> PResult<&[Token], &str> {
    if let Some(Token::Ident(str)) = tl.get(0) {
        Ok((&tl[1..], str.as_str()))
    } else {
        Err(tl)
    }
}

pub fn literal(tl: &[Token]) -> PResult<&[Token], Literal> {
    if let Some(Token::Literal(lit)) = tl.get(0) {
        Ok((&tl[1..], *lit))
    } else {
        Err(tl)
    }
}

pub fn ty(tl: &[Token]) -> PResult<&[Token], Type> {
    let (new_tl, ident) = ident(tl)?;

    match ident {
        "f32" => Ok((new_tl, Type::F32)),
        "i32" => Ok((new_tl, Type::I32)),
        "i64" => Ok((new_tl, Type::I64)),
        "f64" => Ok((new_tl, Type::F64)),

        _ => Err(tl),
    }
}

