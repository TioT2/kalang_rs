mod all;
mod any;
mod str;

use std::marker::PhantomData;

pub use all::*;
pub use any::*;
pub use str::*;

/// Parser work result type
/// * 't - lifetime of destination object
/// * D - parsed object
/// * O - output type
pub type PResult<'t, D, O> = Result<(D, O), D>;

/// Parser trait
pub trait Parser<'t, D: 't, O>: Fn(D) -> PResult<'t, D, O> {
}

/// Implementation of parser trait for parsing function
impl<'t, D: 't, O, F: Fn(D) -> PResult<'t, D, O>> Parser<'t, D, O> for F {
}

/// Identity parser (don't changes anything in input data and yields ())
pub fn identity<'t, D: 't>(data: D) -> PResult<'t, D, ()> {
    Ok((data, ()))
} // fn identity

/// Parser mapping function
/// * parser - parser to map output of
/// * f      - mapping function
pub fn map<'t, D: 't, I, O>(
    parser: impl Parser<'t, D, I>,
    f: impl Fn(I) -> O
) -> impl Parser<'t, D, O> {
    move |str: D| -> PResult<'t, D, O> {
        parser(str).map(|(s, i)| (s, f(i)))
    }
} // fn map

/// Parsing result ignore function
/// * parser - parser to ignore value of
pub fn ignore<'t, D: 't, I>(
    parser: impl Parser<'t, D, I>
) -> impl Parser<'t, D, ()> {
    move |str: D| -> PResult<'t, D, ()> {
        parser(str).map(|(s, _)| (s, ()))
    }
} // fn ignore

/// Set value to void parser (example: literal)
pub fn value<'t, D: 't, O: Clone>(
    parser: impl Parser<'t, D, ()>,
    value: O,
) -> impl Parser<'t, D, O> {
    move |str: D| -> PResult<'t, D, O> {
        parser(str).map(|(s, _)| (s, value.clone()))
    }
} // fn value

/// Filtering function
/// * parser - parser to filter 
/// * f      - function to filter result of parser by
pub fn filter<'t, D: 't + Clone, O>(
    parser: impl Parser<'t, D, O>,
    f: impl Fn(&O) -> bool
) -> impl Parser<'t, D, O> {
    move |str: D| -> PResult<'t, D, O> {
        if let Ok((s, o)) = parser(str.clone()) {
            if f(&o) {
                return Ok((s, o));
            }
        };

        return Err(str);
    }
} // fn filter

/// Actually, combination of filter and map combinators.
/// <result>.is_some() is actually filter function test
pub fn filter_map<'t, D: 't + Clone, I, O>(
    parser: impl Parser<'t, D, I>,
    f: impl Fn(I) -> Option<O>
) -> impl Parser<'t, D, O> {
    move |tl: D| -> PResult<'t, D, O> {
        if let Ok((ntl, i)) = parser(tl.clone()) {
            if let Some(o) = f(i) {
                return Ok((ntl, o));
            }
        }
        return Err(tl);
    }
} // fn filter_map

pub fn or<'t, D: 't + Clone, T>(
    parser: impl Parser<'t, D, T>,
    or_fn: impl Fn() -> T,
) -> impl Parser<'t, D, T> {
    move |tl: D| -> PResult<'t, D, T> {
        match parser(tl.clone()) {
            Ok(v) => Ok(v),
            Err(_) => Ok((tl, or_fn())),
        }
    }
}

pub fn repeat<'t, D: 't + Clone, T, O>(
    parser: impl Parser<'t, D, T>,
    initializer: impl Fn() -> O,
    fold_fn: impl Fn(O, T) -> O
) -> impl Parser<'t, D, O> {
    move |mut str: D| -> PResult<'t, D, O> {
        let mut result = initializer();

        loop {
            let Ok((new_str, value)) = parser(str.clone()) else {
                return Ok((str, result))
            };

            str = new_str;
            result = fold_fn(result, value);
        }
    }
}

pub fn collect_repeat<'t, O: FromIterator<T>, T, D: 't + Clone>(
    parser: impl Parser<'t, D, T>
) -> impl Parser<'t, D, O> {
    struct CrIterator<'b, 't, D: 'b + 't + Clone, T, P: Parser<'t, D, T>> {
        str: &'b mut D,
        parse_fn: &'b P,
        _phd: std::marker::PhantomData<T>,
        _td: std::marker::PhantomData<&'t ()>,
    }

    impl<'b, 't, D: 'b + 't + Clone, T, P: Parser<'t, D, T>> Iterator for CrIterator<'b, 't, D, T, P> {

        type Item = T;

        fn next(&mut self) -> Option<Self::Item> {
            let t;
            (*self.str, t) = (*self.parse_fn)(self.str.clone()).ok()?;
            Some(t)
        }
    }

    move |mut str: D| -> PResult<'t, D, O> {
        let res = O::from_iter(CrIterator {
            _phd: PhantomData::default(),
            _td: PhantomData::default(),
            parse_fn: &parser,
            str: &mut str,
        });

        Ok((str, res))
    }
}

pub fn repeat_with_separator<'t, D: 't + Clone, T, O>(
    value: impl Parser<'t, D, T>,
    separator: impl Parser<'t, D, ()>,
    initializer: impl Fn() -> O,
    fold_fn: impl Fn(O, T) -> O
) -> impl Parser<'t, D, O> {
    move |str: D| -> PResult<'t, D, O> {
        let mut result = initializer();

        let Ok((str, first)) = value(str.clone()) else {
            return Ok((str, result));
        };

        result = fold_fn(result, first);

        let mut str = str;

        loop {
            let Ok((separator_str, _)) = separator(str.clone()) else {
                return Ok((str, result));
            };

            let Ok((next_str, value)) = value(separator_str.clone()) else {
                return Ok((separator_str, result));
            };

            result = fold_fn(result, value);

            str = next_str;
        }
    }
}

// lib.rs
