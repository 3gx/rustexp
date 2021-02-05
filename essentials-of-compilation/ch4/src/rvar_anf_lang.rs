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

use rvar_lang::Expr as RvarExpr;
use rvar_lang::{gensym, sym_get, sym_set, BinaryOpKind, Env, UnaryOpKind, Value};

type Int = i64;
type Bool = bool;
#[derive(Debug, Clone)]
pub enum Atom {
    Int(Int),
    Bool(Bool),
    Var(String),
}
pub macro int($e:expr) {
    Atom::Int($e)
}
pub macro var {
    ($id:ident) => { var!(stringify!($id)) },
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

    Let(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),

    Read,
    UnaryOp(UnaryOpKind, Atom),
    BinaryOp(BinaryOpKind, Atom, Atom),
}

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
            x@(Atom::Bool(_), Some(_)) => panic!("unsuppoted combo {:?}", x),
        }
    }
    match e {
        RvarExpr::Int(i) => Expr::Atom(int!(*i)),
        RvarExpr::Bool(b) => Expr::Atom(Atom::Bool(*b)),
        RvarExpr::Var(x) => Expr::Atom(var!(&x)),
        RvarExpr::Read => Expr::Read,
        RvarExpr::BinaryOp(op, e1, e2) => rco_op(rco_atom(e1), |x| {
            rco_op(rco_atom(e2), |y| Expr::BinaryOp(*op, x, y))
        }),
        RvarExpr::UnaryOp(op, expr) => rco_op(rco_atom(expr), |x| Expr::UnaryOp(*op, x)),
        RvarExpr::Let(x, e, body) => Expr::Let(x.clone(), bx![rco_exp(e)], bx![rco_exp(body)]),
        RvarExpr::If(e1, e2, e3) => Expr::If(bx![rco_exp(e1)], bx![rco_exp(e2)], bx![rco_exp(e3)]),
    }
}

pub fn interp_atom(env: &Env, e: &Atom) -> Value {
    match e {
        Atom::Int(n) => Value::Int(*n),
        Atom::Bool(b) => Value::Bool(*b),
        Atom::Var(x) => sym_get(env, &x).unwrap().clone(),
    }
}
pub fn interp_exp(env: &Env, e: &Expr) -> Value {
    use rvar_lang::CmpOpKind as C;
    use {BinaryOpKind::*, UnaryOpKind::*};
    match e {
        Expr::Atom(atom) => interp_atom(env, atom),
        Expr::Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            Value::Int(input.trim().parse().unwrap())
        }
        Expr::UnaryOp(Neg, e) => Value::Int(-interp_atom(env, e).int().unwrap()),
        Expr::BinaryOp(Add, e1, e2) => {
            Value::Int(interp_atom(env, e1).int().unwrap() + interp_atom(env, e2).int().unwrap())
        }
        Expr::Let(x, e, body) => {
            let new_env = sym_set(env, x, &interp_exp(env, e));
            interp_exp(&new_env, body)
        }
        Expr::UnaryOp(Not, a) => Value::Bool(!interp_atom(env, a).bool().unwrap()),
        Expr::BinaryOp(And, a1, a2) => {
            if *interp_atom(env, a1).bool().unwrap() {
                return Value::Bool(*interp_atom(env, a2).bool().unwrap());
            } else {
                return Value::Bool(false);
            }
        }
        Expr::BinaryOp(Or, a1, a2) => {
            if *interp_atom(env, a1).bool().unwrap() {
                return Value::Bool(true);
            } else {
                return Value::Bool(*interp_atom(env, a2).bool().unwrap());
            }
        }
        Expr::If(e1, e2, e3) => {
            if *interp_exp(env, e1).bool().unwrap() {
                interp_exp(env, e2)
            } else {
                interp_exp(env, e3)
            }
        }
        Expr::BinaryOp(BinaryOpKind::CmpOp(op), a1, a2) => {
            match (op, interp_atom(env, a1), interp_atom(env, a2)) {
                (C::Eq, Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
                (C::Eq, Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
                (C::Le, Value::Int(a), Value::Int(b)) => Value::Bool(a <= b),
                (C::Lt, Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                (C::Ge, Value::Int(a), Value::Int(b)) => Value::Bool(a >= b),
                (C::Gt, Value::Int(a), Value::Int(b)) => Value::Bool(a > b),
                x @ _ => panic!("type mismatch: {:?}", x),
            }
        }
    }
}
