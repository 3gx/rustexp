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
