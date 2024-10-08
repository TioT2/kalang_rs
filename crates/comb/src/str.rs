use crate::{PResult, Parser};

/// String literal parser
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
} // fn literal

/// Unsigned integer parser
/// * base           - resulting number base
/// * char_to_number - single digit parsing function
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
} // fn integer

/// Any character parser (actually, fails only then input ended)
pub fn any_char<'t>(input: &'t str) -> PResult<&'t str, char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        None => Err(input),
    }
} // fn any_char

/// Pairwise character repeats
pub fn any_with_next<'t>(input: &'t str) -> PResult<&'t str, (char, char)> {
    let mut chs = input.chars();
    let c0 = chs.next().ok_or(input)?;
    let c1 = chs.next().ok_or(input)?;

    Ok((&input[c0.len_utf8()..], (c0, c1)))
} // fn pairwise

/// Any whitespace character parser
pub fn any_whitespace<'t>(input: &'t str, ) -> PResult<&'t str, char> {
    if let Some(next) = input.chars().next().filter(|v| v.is_whitespace()) {
        Ok((&input[next.len_utf8()..], next))
    } else {
        Err(input)
    }
} // fn any_whitespace

/// Hexadecimal (base-16) number parser
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
} // fn hexadecimal_number

/// Octal (base-8) number parser
pub fn octal_number<'t>(str: &'t str) -> PResult<&'t str, u64> {
    fn char_to_oct_number(ch: char) -> Option<u64> {
        match ch {
            '0'..='7' => Some(ch as u64 - '0' as u64),
            _ => None,
        }
    }

    integer(8, char_to_oct_number)(str)
} // fn octal_number

/// Decimal (base-10) number parser
pub fn decimal_number<'t>(str: &'t str) -> PResult<&'t str, u64> {
    fn char_to_decimal_number(ch: char) -> Option<u64> {
        match ch {
            '0'..='9' => Some(ch as u64 - '0' as u64),
            _ => None
        }
    }

    integer(10, char_to_decimal_number)(str)
} // fn decimal_number

/// Binary (base-2) number parsing
pub fn binary_number<'t>(str: &'t str) -> PResult<&'t str, u64> {
    fn char_to_binary_number(ch: char) -> Option<u64> {
        match ch {
            '0' => Some(0),
            '1' => Some(1),
            _   => None,
        }
    }

    integer(2, char_to_binary_number)(str)
} // fn binary_number

/// FP number parsing function
pub fn floating_number<'t>(mut str: &'t str) -> PResult<&'t str, f64> {
    /// Sign parsing function
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

// str.rs
