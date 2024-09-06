use super::{Parser, PResult};

pub trait AnyHelper<'t, T> {
    fn parse(&self, str: &'t str) -> PResult<'t, T>;
}

macro_rules! impl_any_helper_impl {
    ($([$pt: ident, $pn: ident], )*) => {
        impl<'t, O, $($pt: Parser<'t, O>),*> AnyHelper<'t, O> for ($($pt),*) {
            fn parse(&self, str: &'t str) -> PResult<'t, O> {
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

pub fn any<'t, T>(any: impl AnyHelper<'t, T>) -> impl Parser<'t, T> {
    move |str: &'t str| -> PResult<'t, T> {
        any.parse(str)
    }
}
