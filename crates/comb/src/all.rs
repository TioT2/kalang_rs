use super::{Parser, PResult};

pub trait AllHelper<'t, T> {
    fn parse(&self, str: &'t str) -> PResult<'t, T>;
}

macro_rules! impl_all_helper_impl {
    ($([$tn: ident, $on: ident, $rn: ident, $pi: ident], )*) => {
        impl <'t, $($tn: Parser<'t, $on>, $on),*> AllHelper<'t, ($($on),*)> for ($($tn),*) {
            fn parse(&self, str: &'t str) -> PResult<'t, ($($on),*)> {
                let ($($pi),*) = self;
                $(
                    let (str, $rn) = $pi(str)?;
                )*

                Ok((
                    str,
                    ($($rn),*)
                ))
            }
        }
    };
}

macro_rules! impl_all_helper {
    ([$htn: ident, $hon: ident, $hrn: ident, $hpi: ident], ) => {

    };

    ([$htn: ident, $hon: ident, $hrn: ident, $hpi: ident], $([$tn: ident, $on: ident, $rn: ident, $pi: ident],)*) => {
        impl_all_helper_impl!(
            [$htn, $hon, $hrn, $hpi],
            $([$tn, $on, $rn, $pi], )*
        );

        impl_all_helper!(
            $([$tn, $on, $rn, $pi], )*
        );
    };
}

impl_all_helper!(
    [T00, O00, r00, p00],
    [T01, O01, r01, p01],
    [T02, O02, r02, p02],
    [T03, O03, r03, p03],
    [T04, O04, r04, p04],
    [T05, O05, r05, p05],
    [T06, O06, r06, p06],
    [T07, O07, r07, p07],
    [T08, O08, r08, p08],
    [T09, O09, r09, p09],
    [T10, O10, r10, p10],
    [T11, O11, r11, p11],
    [T12, O12, r12, p12],
    [T13, O13, r13, p13],
    [T14, O14, r14, p14],
    [T15, O15, r15, p15],
    [T16, O16, r16, p16],
    [T17, O17, r17, p17],
    [T18, O18, r18, p18],
    [T19, O19, r19, p19],
);

pub fn all<'t, T>(tup: impl AllHelper<'t, T>) -> impl Parser<'t, T> {
    move |str: &'t str| -> PResult<'t, T> {
        tup.parse(str)
    }
}
