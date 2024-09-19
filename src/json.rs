use std::collections::HashMap;

use comb::{self, all, any, filter, map, repeat, repeat_with_separator, value, PResult};

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
fn quoted_string(str: &str) -> PResult<&str, String> {
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
fn parse_string(str: &str) -> PResult<&str, Json> {
    map(
        quoted_string,
        Json::String
    )(str)
} // fn parse_string

/// bool parsing function
fn parse_bool(str: &str) -> PResult<&str, Json> {
    map(
        any((
            value(comb::literal("true" ), true ),
            value(comb::literal("false"), false)
        )),
        Json::Boolean
    )(str)
} // fn parse_bool

/// NULL parse function
fn parse_null(str: &str) -> PResult<&str, Json> {
    value(comb::literal("null"), Json::Null)(str)
} // fn parse_null

/// whitespace sequence parsing function
fn whitespace<'a>(str: &str) -> PResult<&str, ()> {
    repeat(
        filter(comb::any_char, |c| c.is_whitespace()),
        || (),
        |_, _| ()
    )(str)
} // fn whitespace

/// Whitespace-surrounding function
fn surround<'t, T>(parser: impl comb::Parser<'t, &'t str, T>) -> impl comb::Parser<'t, &'t str, T> {
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
fn parse_array(str: &str) -> PResult<&str, Json> {
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

/// Object parsing function
fn parse_object(str: &str) -> PResult<&str, Json> {
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

/// JSON number parsing function
fn parse_number(str: &str) -> PResult<&str, Json> {
    map(
        comb::floating_number,
        Json::Number
    )(str)
} // fn parse_number

/// JSON parsing function
fn parse_json(str: &str) -> PResult<&str, Json> {
    any((
        parse_string,
        parse_bool,
        parse_null,
        parse_array,
        parse_object,
        parse_number
    ))(str)
} // fn parse_json

// file json.rs