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

use rvar_lang as RVar;
use RVar::Expr as RVarExpr;
pub use RVar::{gensym, sym_get, sym_set, Env, UnaryOpKind, Value};

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum BinaryOpKind {
    Add,
    Eq,
    Lt,
}

pub type Int = i64;
pub type Bool = bool;
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

fn rco_atom(e: &RVarExpr) -> (Atom, Option<Expr>) {
    match e {
        RVarExpr::Int(i) => (int!(*i), None),
        RVarExpr::Var(x) => (var!(&x), None),
        e => (var!(gensym("tmp")), Some(rco_exp(e))),
    }
}
// remove-complex-opera* {opera* = operations|operands}
pub fn rco_exp(e: &RVarExpr) -> Expr {
    let rco_op = |(a, e): (Atom, Option<Expr>), f: &dyn Fn(Atom) -> Expr| -> Expr {
        r#match! { (a, e),
            (a, None) => f(a.clone()),
            (Atom::Var(x), Some(e)) => {
                let a = var!(&x);
                Expr::Let(x, bx![e], bx![f(a)])
            }
            x@(Atom::Int(_), Some(_)) => panic!("unsuppoted combo {:?}", x),
            x@(Atom::Bool(_), Some(_)) => panic!("unsuppoted combo {:?}", x),
        }
    };
    let simplify_and_rco_binop = |op: &RVar::BinaryOpKind, e1: &RVarExpr, e2: &RVarExpr| -> Expr {
        use RVar::BinaryOpKind as RVarOpKind;
        use RVar::CmpOpKind as RVarCmpKind;
        let rco_op_apply = |op, e1, e2| {
            rco_op(rco_atom(e1), &|x| {
                rco_op(rco_atom(e2), &|y| Expr::BinaryOp(op, x.clone(), y.clone()))
            })
        };
        match op {
            RVarOpKind::And => rco_exp(&RVarExpr::If(e1.bx(), e2.bx(), RVarExpr::Bool(false).bx())),
            RVarOpKind::Or => rco_exp(&RVarExpr::If(e1.bx(), RVarExpr::Bool(true).bx(), e2.bx())),
            RVarOpKind::CmpOp(op) => match op {
                RVarCmpKind::Eq => rco_op_apply(BinaryOpKind::Eq, e1, e2),
                RVarCmpKind::Lt => rco_op_apply(BinaryOpKind::Lt, e1, e2),
                RVarCmpKind::Le => unimplemented!(),
                RVarCmpKind::Gt => unimplemented!(),
                RVarCmpKind::Ge => unimplemented!(),
            },
            RVarOpKind::Add => rco_op_apply(BinaryOpKind::Add, e1, e2),
        }
    };
    match e {
        RVarExpr::Int(i) => Expr::Atom(int!(*i)),
        RVarExpr::Bool(b) => Expr::Atom(Atom::Bool(*b)),
        RVarExpr::Var(x) => Expr::Atom(var!(&x)),
        RVarExpr::Read => Expr::Read,
        RVarExpr::BinaryOp(op, e1, e2) => simplify_and_rco_binop(op, e1, e2),
        RVarExpr::UnaryOp(op, expr) => rco_op(rco_atom(expr), &|x| Expr::UnaryOp(*op, x)),
        RVarExpr::Let(x, e, body) => Expr::Let(x.clone(), bx![rco_exp(e)], bx![rco_exp(body)]),
        RVarExpr::If(e1, e2, e3) => Expr::If(bx![rco_exp(e1)], bx![rco_exp(e2)], bx![rco_exp(e3)]),
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
    match e {
        Expr::Atom(atom) => interp_atom(env, atom),
        Expr::Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            Value::Int(input.trim().parse().unwrap())
        }
        Expr::Let(x, e, body) => {
            let new_env = sym_set(env, x, &interp_exp(env, e));
            interp_exp(&new_env, body)
        }
        Expr::If(e1, e2, e3) => {
            if *interp_exp(env, e1).bool().unwrap() {
                interp_exp(env, e2)
            } else {
                interp_exp(env, e3)
            }
        }
        Expr::UnaryOp(op, a) => match (op, interp_atom(env, a)) {
            (UnaryOpKind::Not, Value::Bool(b)) => Value::Bool(!b),
            (UnaryOpKind::Neg, Value::Int(i)) => Value::Int(-i),
            x @ _ => panic!("type mismatch: {:?}", x),
        },
        Expr::BinaryOp(op, a1, a2) => match (op, interp_atom(env, a1), interp_atom(env, a2)) {
            (BinaryOpKind::Add, Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (BinaryOpKind::Eq, Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
            (BinaryOpKind::Eq, Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
            (BinaryOpKind::Lt, Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
            x @ _ => panic!("type mismatch: {:?}", x),
        },
    }
}
