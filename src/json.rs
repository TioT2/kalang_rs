use std::collections::HashMap;

use comb::{self, all, any, filter, repeat, map, repeat_with_separator, PResult};

/// JSON element representation sturcture
#[derive(Clone, Debug)]
pub enum Json {
    /// String
    String(String),

    /// Number
    Number(f64),

    /// Boolean
    Boolean(bool),

    /// JSON object array
    Array(Vec<Json>),

    /// String->Json object map
    Object(HashMap<String, Json>),

    /// Null
    Null,
} // enum Json

impl Json {
    /// JSON parsing function
    pub fn parse(json: &str) -> Option<Json> {
        match parse_json(json) {
            Ok((_, res)) => Some(res),
            Err(_) => None,
        }
    } // fn parse
} // impl Json

/// Quoted string parsing function
fn quoted_string(str: &str) -> PResult<String> {
    map(
        all((
            comb::literal("\""),
            repeat(
                filter(
                    comb::any_char,
                    |c| *c != '\"'
                ),
                String::new,
                |mut string, char| {
                    string.push(char);
                    string
                }
            ),
            comb::literal("\""),
        )),
        |(_, v, _)| v,
    )(str)
} // fn quoted_string

/// JSON string parsing function
fn parse_string(str: &str) -> PResult<Json> {
    map(
        quoted_string,
        Json::String
    )(str)
} // fn parse_string

/// bool parsing function
fn parse_bool(str: &str) -> PResult<Json> {
    map(
        any((
            map(comb::literal("true" ), |_| true ),
            map(comb::literal("false"), |_| false)
        )),
        Json::Boolean
    )(str)
} // fn parse_bool

/// NULL parse function
fn parse_null(str: &str) -> PResult<Json> {
    map(
        comb::literal("null"),
        |_| Json::Null
    )(str)
} // fn parse_null

/// whitespace sequence parsing function
fn whitespace<'a>(str: &str) -> PResult<()> {
    repeat(
        filter(comb::any_char, |c| c.is_whitespace()),
        || (),
        |_, _| ()
    )(str)
} // fn whitespace

/// Whitespace-surrounding function
fn surround<'a, T>(parser: impl comb::Parser<'a, T>) -> impl comb::Parser<'a, T> {
    map(
        all((
            whitespace,
            parser,
            whitespace
        )),
        |(_, r, _)| r
    )
} // fn surround

/// JSON array parsing function
fn parse_array<'t>(str: &'t str) -> PResult<Json> {
    map(
        all((
            comb::literal("["),
            repeat_with_separator(
                surround(parse_json),
                comb::literal(","),
                Vec::new,
                |mut array, json| {
                    array.push(json);
                    array
                }
            ),
            comb::literal("]"),
        )),
        |(_, v, _)| Json::Array(v),
    )(str)
} // fn parse_array

fn parse_object<'t>(str: &'t str) -> PResult<Json> {
    let object_pair = map(
        all((
            quoted_string,
            surround(comb::literal(":")),
            parse_json
        )),
        |(name, _, json)| (name, json)
    );

    let object_contents = repeat_with_separator(
        surround(object_pair),
        surround(comb::literal(",")),
        HashMap::new,
        |mut map, (name, json)| {
            map.insert(name, json);
            map
        }
    );

    map(
        all((
            surround(comb::literal("{")),
            map(
                object_contents,
                Json::Object
            ),
            surround(comb::literal("}")),
        )),
        |(_, v, _)| v
    )(str)
} // fn parse_object

fn parse_number(str: &str) -> PResult<Json> {
    map(
        comb::floating_number,
        Json::Number
    )(str)
}

fn parse_json(str: &str) -> PResult<Json> {
    any((
        parse_string,
        parse_bool,
        parse_null,
        parse_array,
        parse_object,
        parse_number
    ))(str)
}
