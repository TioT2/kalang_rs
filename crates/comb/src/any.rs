use super::{Parser, PResult};

pub trait AnyHelper<'t, D, T> {
    fn parse(&self, str: D) -> PResult<'t, D, T>;
}

macro_rules! impl_any_helper_impl {
    ($([$pt: ident, $pn: ident], )*) => {
        impl<'t, D: 't + Copy, O, $($pt: Parser<'t, D, O>),*> AnyHelper<'t, D, O> for ($($pt),*) {
            fn parse(&self, str: D) -> PResult<'t, D, O> {
                let ($($pn),*) = self;

                $(
                    if let Ok(result) = $pn(str) {
                        return Ok(result);
                    }
                )*

                return Err(str);
            }
        }
    }
}

macro_rules! impl_any_helper {
    ([$pt: ident, $pn: ident],) => {

    };

    ([$hpt: ident, $hpn: ident], $([$pt: ident, $pn: ident], )*) => {
        impl_any_helper_impl!(
            [$hpt, $hpn],
            $(
                [$pt, $pn],
            )*
        );

        impl_any_helper!($([$pt, $pn],)*);
    };
}

impl_any_helper!(
    [P00, p00],
    [P01, p01],
    [P02, p02],
    [P03, p03],
    [P04, p04],
    [P05, p05],
    [P06, p06],
    [P07, p07],
    [P08, p08],
    [P09, p09],

    [P10, p10],
    [P11, p11],
    [P12, p12],
    [P13, p13],
    [P14, p14],
    [P15, p15],
    [P16, p16],
    [P17, p17],
    [P18, p18],
    [P19, p19],
);

pub fn any<'t, D: 't, T>(any: impl AnyHelper<'t, D, T>) -> impl Parser<'t, D, T> {
    move |str: D| -> PResult<'t, D, T> {
        any.parse(str)
    }
}

pub fn any_or<'t, D: 't + Clone, T>(any: impl AnyHelper<'t, D, T>, or: impl Fn() -> T) -> impl Parser<'t, D, T> {
    move |tl: D| -> PResult<'t, D, T> {
        Ok(match any.parse(tl.clone()) {
            Ok(v) => v,
            Err(_) => (tl, or()),
        })
    }
}