#[path = "./macros.rs"]
mod macros;
use macros::bx;
//use macros::r#match;

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
use RVar::TExpr as RVarTExpr;
use RVar::TypedExpr as RVarExpr;
pub use RVar::{gensym, sym_get, sym_set, Env, Type, UnaryOpKind, Value};

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
    Void,
}
/*
pub macro int($e:expr) {
    Atom::Int($e)
}
pub macro var {
    ($id:ident) => { Atom::Var(stringify!($id).to_string()) },
    ($e:expr) => { $e.to_string().as_str().into_term() },
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
*/

#[derive(Debug, Clone)]
pub enum ExprK {
    Atom(Atom),

    Let(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),

    Read,
    UnaryOp(UnaryOpKind, Atom),
    BinaryOp(BinaryOpKind, Atom, Atom),

    Collect(Int),
    Allocate(Int, Type),
    GlobalValue(String),
}
impl ExprK {
    fn expr(self, ty: Type) -> Expr {
        Expr(self, ty)
    }
}
#[derive(Debug, Clone)]
pub struct Expr(pub ExprK, pub Type);

fn rco_atom(expr: &RVarExpr) -> ((Atom, Type), Option<Expr>) {
    let RVarExpr(e, _) = expr;
    match e {
        RVarTExpr::Int(i) => ((Atom::Int(*i), expr.1.clone()), None),
        RVarTExpr::Var(x) => ((Atom::Var(x.clone()), expr.1.clone()), None),
        _ => (
            (Atom::Var(gensym("tmp")), expr.1.clone()),
            Some(rco_exp(expr)),
        ),
    }
}
// remove-complex-opera* {opera* = operations|operands}
pub fn rco_exp(RVarExpr(e, ty): &RVarExpr) -> Expr {
    let rco_op = |(a, e): ((Atom, Type), Option<Expr>), f: &dyn Fn((Atom, Type)) -> Expr| -> Expr {
        match (a, e) {
            (a, None) => f(a.clone()),
            ((Atom::Var(x), var_ty), Some(e)) => {
                let Expr(a, a_ty) = f((Atom::Var(x.clone()), var_ty));
                Expr(ExprK::Let(x, bx![e], bx![Expr(a, a_ty)]), ty.clone())
            }
            x @ ((Atom::Int(_), _), Some(_)) => panic!("unsuppoted combo {:?}", x),
            x @ ((Atom::Bool(_), _), Some(_)) => panic!("unsuppoted combo {:?}", x),
            ((Atom::Void, _), _) => unimplemented!(),
        }
    };

    let simplify_and_rco_binop =
        |op: &RVar::BinaryOpKind, e1: &RVarExpr, e2: &RVarExpr, ty: Type| -> Expr {
            use RVar::BinaryOpKind as RVarOpKind;
            use RVar::CmpOpKind as RVarCmpKind;
            let rco_op_apply = |op, e1, e2| {
                rco_op(rco_atom(e1), &|(x, _xty)| {
                    rco_op(rco_atom(e2), &|(y, _yty)| {
                        Expr(ExprK::BinaryOp(op, x.clone(), y.clone()), ty.clone())
                    })
                })
            };
            match op {
                RVarOpKind::And => rco_exp(
                    &RVarTExpr::If(e1.bx(), e2.bx(), RVarTExpr::Bool(false).tbx(Type::Bool))
                        .texpr(Type::Bool),
                ),
                RVarOpKind::Or => rco_exp(
                    &RVarTExpr::If(e1.bx(), RVarTExpr::Bool(true).tbx(Type::Bool), e2.bx())
                        .texpr(Type::Bool),
                ),
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

    let ty = ty.clone();
    match e {
        RVarTExpr::Int(i) => ExprK::Atom(Atom::Int(*i)).expr(ty),
        RVarTExpr::Bool(b) => ExprK::Atom(Atom::Bool(*b)).expr(ty),
        RVarTExpr::Var(x) => ExprK::Atom(Atom::Var(x.clone())).expr(ty),
        RVarTExpr::Read => ExprK::Read.expr(ty),
        RVarTExpr::BinaryOp(op, e1, e2) => simplify_and_rco_binop(op, e1, e2, ty),
        RVarTExpr::UnaryOp(op, expr) => {
            rco_op(rco_atom(expr), &|(x, ty)| ExprK::UnaryOp(*op, x).expr(ty))
        }
        RVarTExpr::Let(x, e, body) => {
            ExprK::Let(x.clone(), bx![rco_exp(e)], bx![rco_exp(body)]).expr(ty)
        }
        RVarTExpr::If(e1, e2, e3) => {
            ExprK::If(bx![rco_exp(e1)], bx![rco_exp(e2)], bx![rco_exp(e3)]).expr(ty)
        }
        RVarTExpr::Tuple(..) => unimplemented!(),
        RVarTExpr::TupleLen(..) => unimplemented!(),
        RVarTExpr::TupleRef(..) => unimplemented!(),
        RVarTExpr::TupleSet(..) => unimplemented!(),
        RVarTExpr::Void => unimplemented!(),
        RVarTExpr::HasType(..) => unimplemented!(),
    }
}

pub fn interp_atom(env: &Env, e: &Atom) -> Value {
    match e {
        Atom::Int(n) => Value::Int(*n),
        Atom::Bool(b) => Value::Bool(*b),
        Atom::Var(x) => sym_get(env, &x).unwrap().clone(),
        Atom::Void => unimplemented!(),
    }
}
pub fn interp_exp(env: &Env, Expr(e, _): &Expr) -> Value {
    match e {
        ExprK::Atom(atom) => interp_atom(env, atom),
        ExprK::Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            Value::Int(input.trim().parse().unwrap())
        }
        ExprK::Let(x, e, body) => {
            let new_env = sym_set(env, x, &interp_exp(env, e));
            interp_exp(&new_env, body)
        }
        ExprK::If(e1, e2, e3) => {
            if *interp_exp(env, e1).bool().unwrap() {
                interp_exp(env, e2)
            } else {
                interp_exp(env, e3)
            }
        }
        ExprK::UnaryOp(op, a) => match (op, interp_atom(env, a)) {
            (UnaryOpKind::Not, Value::Bool(b)) => Value::Bool(!b),
            (UnaryOpKind::Neg, Value::Int(i)) => Value::Int(-i),
            x @ _ => panic!("type mismatch: {:?}", x),
        },
        ExprK::BinaryOp(op, a1, a2) => match (op, interp_atom(env, a1), interp_atom(env, a2)) {
            (BinaryOpKind::Add, Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (BinaryOpKind::Eq, Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
            (BinaryOpKind::Eq, Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
            (BinaryOpKind::Lt, Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
            x @ _ => panic!("type mismatch: {:?}", x),
        },
        ExprK::Allocate(..) => unimplemented!(),
        ExprK::Collect(..) => unimplemented!(),
        ExprK::GlobalValue(..) => unimplemented!(),
    }
}
