#[path = "./macros.rs"]
mod macros;
pub use macros::__mk_op;
//use macros::{bx, r#match, IntoExpr};
use macros::IntoTerm;

#[path = "rvar_lang.rs"]
pub mod rvar_lang;

type Int = i64;
#[derive(Debug, Clone)]
pub enum Atom {
    Int(Int),
    Var(String),
}
pub macro int($e:expr) {
    Atom::Int($e)
}
impl IntoTerm<Atom> for Int {
    fn into_term(&self) -> Atom {
        int!(*self)
    }
}
impl IntoTerm<Atom> for &str {
    fn into_term(&self) -> Atom {
        Atom::Var(self.to_string())
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(Atom),
    Read,
    Neg(Atom),
    Add(Atom, Atom),
    Let(String, Box<Expr>, Box<Expr>),
}

pub macro read() {
    Expr::Read
}
pub macro add {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*) (@expr (@ctor Expr::Add))) }
}

pub macro neg {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*) (@expr (@ctor Expr::Neg)) ) }
}

use rvar_lang::{gensym, gensym_reset, sym, sym_get, sym_set};
