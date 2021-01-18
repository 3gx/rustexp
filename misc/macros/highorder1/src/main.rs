#[macro_export]
macro_rules! fold {
    ($op:ident, $($e:expr),+) => {
        fold_impl! {
            exprs = [{ $($e),+ }]
            binop = [{ $op }]
        }
    };
    ($call:tt!(), $($e:expr),+) => {
        tt_call::tt_call! {
            macro = [{ $call }]
            ~~> fold_impl! {
                exprs = [{ $($e),+ }]
            }
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! fold_impl {
    {
        exprs = [{ $single:expr }]
        binop = [{ $op:ident }]
    } => {
        $single
    };
    {
        exprs = [{ $first:expr, $($rest:expr),+ }]
        binop = [{ $op:ident }]
    } => {
        $op!($first, $crate::fold_impl! {
            exprs = [{ $($rest),+ }]
            binop = [{ $op }]
        })
    };
}

macro_rules! add {
    ($a:expr, $b:expr) => {
        std::ops::Add::add($a, $b)
    };
}

macro_rules! get_add {
    ($caller:tt) => {
        tt_call::tt_return! {
            $caller
            binop = [{ add }]
        }
    };
}

fn main() {
    let out =  fold!(get_add!(), 1u32, 2u32, 3u32);
    println!("{}", out);
}
