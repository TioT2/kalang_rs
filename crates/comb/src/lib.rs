mod all;
mod any;

pub use all::*;
pub use any::*;

pub type PResult<'t, D, O> = Result<(D, O), D>;

pub trait Parser<'t, D: 't, O>: Fn(D) -> PResult<'t, D, O> {
}

impl<'t, D: 't, O, F: Fn(D) -> PResult<'t, D, O>> Parser<'t, D, O> for F {
}

pub fn map<'t, D: 't, I, O>(
    parser: impl Parser<'t, D, I>,
    f: impl Fn(I) -> O
) -> impl Parser<'t, D, O> {
    move |str: D| -> PResult<'t, D, O> {
        parser(str).map(|(s, i)| (s, f(i)))
    }
}

pub fn filter<'t, D: 't + Copy, O>(
    parser: impl Parser<'t, D, O>,
    f: impl Fn(&O) -> bool
) -> impl Parser<'t, D, O> {
    move |str: D| -> PResult<'t, D, O> {
        if let Ok((s, o)) = parser(str) {
            if f(&o) {
                return Ok((s, o));
            }
        };

        return Err(str);
    }
}

pub fn literal<'t>(
    literal: &'static str
) -> impl Parser<'t, &'t str, ()> {
    move |str: &'t str| -> PResult<'t, &'t str, ()> {
        if str.starts_with(literal) {
            Ok((&str[literal.len()..], ()))
        } else {
            Err(str)
        }
    }
}

pub fn repeat<'t, D: 't + Copy, T, O>(
    parser: impl Parser<'t, D, T>,
    initializer: impl Fn() -> O,
    fold_fn: impl Fn(O, T) -> O
) -> impl Parser<'t, D, O> {
    move |mut str: D| -> PResult<'t, D, O> {
        let mut result = initializer();

        loop {
            let Ok((new_str, value)) = parser(str) else {
                return Ok((str, result))
            };

            str = new_str;
            result = fold_fn(result, value);
        }
    }
}

pub fn repeat_with_separator<'t, D: 't + Copy, T, O>(
    value: impl Parser<'t, D, T>,
    separator: impl Parser<'t, D, ()>,
    initializer: impl Fn() -> O,
    fold_fn: impl Fn(O, T) -> O
) -> impl Parser<'t, D, O> {
    move |str: D| -> PResult<'t, D, O> {
        let mut result = initializer();

        let Ok((str, first)) = value(str) else {
            return Ok((str, result));
        };

        result = fold_fn(result, first);

        let mut str = str;

        loop {
            let Ok((separator_str, _)) = separator(str) else {
                return Ok((str, result));
            };

            let Ok((next_str, value)) = value(separator_str) else {
                return Err(separator_str);
            };

            result = fold_fn(result, value);

            str = next_str;
        }
    }
}

pub fn integer<'t>(
    base: u64,
    char_to_number: impl Fn(char) -> Option<u64>
) -> impl Parser<'t, &'t str, u64> {
    move |str: &'t str| -> PResult<'t, &'t str, u64> {
        let mut digits = str
            .chars()
            .map(|char| (char.len_utf8(), char_to_number(char)))
            .take_while(|(_, n)| n.is_some())
            .map(|(len, n)| (len, n.unwrap()));

        let init = digits.next().ok_or(str)?;

        let (len, result) = digits.fold(init, |(parsed_len, result), (digit_len, digit)| {
            (
                parsed_len + digit_len,
                result * base + digit,
            )
        });

        Ok((&str[len..], result))
    }
}

pub fn any_char<'t>(input: &'t str) -> PResult<&'t str, char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        None => Err(input),
    }
}

pub fn any_whitespace<'t>(input: &'t str, ) -> PResult<&'t str, char> {
    if let Some(next) = input.chars().next().filter(|v| v.is_whitespace()) {
        Ok((&input[next.len_utf8()..], next))
    } else {
        Err(input)
    }
}

pub fn hexadecimal_number<'t>(str: &'t str) -> PResult<&'t str, u64> {
    fn char_to_hex_number(ch: char) -> Option<u64> {
        match ch {
            '0'..='9' => Some(ch as u64 - '0' as u64),
            'A'..='F' => Some(ch as u64 - 'A' as u64 + 10),
            'a'..='f' => Some(ch as u64 - 'a' as u64 + 10),
            _         => None
        }
    }

    integer(16, char_to_hex_number)(str)
}

pub fn decimal_number<'t>(str: &'t str) -> PResult<&'t str, u64> {
    fn char_to_decimal_number(ch: char) -> Option<u64> {
        match ch {
            '0'..='9' => Some(ch as u64 - '0' as u64),
            _ => None
        }
    }

    integer(10, char_to_decimal_number)(str)
}

pub fn binary_number<'t>(str: &'t str) -> PResult<&'t str, u64> {
    fn char_to_binary_number(ch: char) -> Option<u64> {
        match ch {
            '0' => Some(0),
            '1' => Some(1),
            _   => None,
        }
    }

    integer(2, char_to_binary_number)(str)
}

pub fn floating_number<'t>(mut str: &'t str) -> PResult<&'t str, f64> {
    fn parse_sign<'t>(str: &'t str) -> PResult<&'t str, i32> {
        match str.chars().next() {
            Some('+' | '.') => Ok((&str['+'.len_utf8()..], 1)),
            Some('0'..='9') => Ok((str, 1)),
            Some('-') => Ok((&str['-'.len_utf8()..], -1)),
            _ => Err(str)
        }
    }

    let sign;
    let integer_part;

    (str, sign) = parse_sign(str)?;
    (str, integer_part) = decimal_number(str)?;

    // str
    let fractional_part = if str.starts_with('.') {
        str = &str[1..];

        let (result, _, index) = str
            .char_indices()
            .take_while(|(_, char)| char.is_numeric())
            .try_fold((0.0f64, 0.1f64, 0), |(res, power, _), (index, char)| {
                let num = match char {
                    '0'..='9' => char as u8 - '0' as u8,
                    _ => unreachable!("HOW THE FUCK IS IT POSSIBLE")
                };

                Some((
                    res + num as f64 * power,
                    power * 0.1,
                    index + 1
                ))
            })
            .unwrap_or((0.0, 0.0, 0));

        str = &str[index..];

        Some(result)
    } else {
        None
    };

    let exponent = if str.starts_with('e') {
        let exp_sign;
        let exp;

        str = &str[1..];

        (str, exp_sign) = parse_sign(str)?;
        (str, exp) = decimal_number(str)?;

        Some(exp as i32 * exp_sign)
    } else {
        None
    };

    if fractional_part.is_none() && exponent.is_none() {
        return Err(str);
    }

    Ok((str, sign as f64 * (integer_part as f64 + fractional_part.unwrap_or(0.0)) * 10.0f64.powi(exponent.unwrap_or(0))))
} // fn floating_number

// lib.rs
