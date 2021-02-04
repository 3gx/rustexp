#[path = "./macros.rs"]
mod macros;
use macros::{bx, r#match};

#[path = "rvar_lang.rs"]
pub mod rvar_lang;

pub trait IntoTerm {
    fn into_term(&self) -> Atom;
}

macro __mk_op {
    ( (@args) (@expr (@ctor $($ctor:tt)*) $($tt:tt)*) ) => { $($ctor)*($($tt)*) },
    ( (@args $i:ident)  (@expr $($tt:tt)*) ) => {
        __mk_op!((@args)
                 (@expr $($tt)* Box::new(stringify!($i).into_term())))
    },
    ( (@args $e:expr)  (@expr $($tt:tt)*) ) => {
        __mk_op!((@args) (@expr $($tt)* Box::new($e.into_term())))
    },
    ( (@args $i:ident, $($tail:tt)*)  (@expr $($tt:tt)*) ) => {
        __mk_op!((@args $($tail)*)
                 (@expr $($tt)* Box::new(stringify!($i).into_term()),))
    },
    ( (@args $e:expr, $($tail:tt)*)  (@expr $($tt:tt)*) ) => {
        __mk_op!((@args $($tail)*) (@expr $($tt)* Box::new($e.into_term()),))
    },
}

type Int = i64;
#[derive(Debug, Clone)]
pub enum Atom {
    Int(Int),
    Var(String),
}
pub macro int($e:expr) {
    Atom::Int($e)
}
pub macro var {
    ($id:ident) => { var!(stringify!($id)) },
//    ($e:expr) => { Atom::Var($e.to_string()) },
    ($e:expr) => { $e.as_str().into_term() },
}
impl IntoTerm for Int {
    fn into_term(&self) -> Atom {
        int!(*self)
    }
}
impl IntoTerm for &str {
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

use rvar_lang::{gensym, sym_get, sym_set, EnvInt};

use rvar_lang::Expr as RvarExpr;

fn rco_atom(e: &RvarExpr) -> (Atom, Option<Expr>) {
    match e {
        RvarExpr::Int(i) => (int!(*i), None),
        RvarExpr::Var(x) => (var!(&x), None),
        e => (var!(gensym("tmp")), Some(rco_exp(e))),
    }
}
// remove-complex-opera* {opera* = operations|operands}
pub fn rco_exp(e: &RvarExpr) -> Expr {
    fn rco_op((a, e): (Atom, Option<Expr>), f: impl FnOnce(Atom) -> Expr) -> Expr {
        r#match! { (a, e),
            (a, None) => f(a.clone()),
            (Atom::Var(x), Some(e)) => {
                let a = var!(&x);
                Expr::Let(x, bx![e], bx![f(a)])
            }
            x@(Atom::Int(_), Some(_)) => panic!("unsuppoted combo {:?}", x),
        }
    }
    match e {
        RvarExpr::Int(i) => Expr::Atom(int!(*i)),
        RvarExpr::Var(x) => Expr::Atom(var!(&x)),
        RvarExpr::Read => Expr::Read,
        RvarExpr::Add(e1, e2) => {
            let (a1, e1) = rco_atom(e1);
            let ae2 = rco_atom(e2);
            rco_op((a1, e1), |x| rco_op(ae2, |y| Expr::Add(x, y)))
        }
        RvarExpr::Neg(e) => rco_op(rco_atom(e), |x| Expr::Neg(x)),
        RvarExpr::Let(x, e, body) => Expr::Let(x.clone(), bx![rco_exp(e)], bx![rco_exp(body)]),
    }
}

pub fn interp_atom(env: &EnvInt, e: &Atom) -> Int {
    match e {
        Atom::Int(n) => *n,
        Atom::Var(x) => sym_get(env, &x).unwrap().clone(),
    }
}
pub fn interp_exp(env: &EnvInt, e: &Expr) -> Int {
    match e {
        Expr::Atom(atom) => interp_atom(env, atom),
        Expr::Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            input.trim().parse().unwrap()
        }
        Expr::Neg(e) => -interp_atom(env, e),
        Expr::Add(e1, e2) => interp_atom(env, e1) + interp_atom(env, e2),
        Expr::Let(x, e, body) => {
            let new_env = sym_set(env, x, &interp_exp(env, e));
            interp_exp(&new_env, body)
        }
    }
}
