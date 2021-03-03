#[path = "./macros.rs"]
mod macros;
//use macros::bx;
//use macros::r#match;

#[path = "rvar_lang.rs"]
pub mod rvar_lang;
use rvar_lang::expr;

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

impl Expr {
    pub fn bx(&self) -> Box<Expr> {
        Box::new(self.clone())
    }
}

// convert to atom
fn rco_atom(RVarExpr(e, ty): RVarExpr) -> (Atom, Option<Expr>) {
    match e {
        // atom's stay atoms
        RVarTExpr::Int(i) => (Atom::Int(i), None),
        RVarTExpr::Bool(b) => (Atom::Bool(b), None),
        RVarTExpr::Var(x) => (Atom::Var(x), None),
        // if e is a complex expression, assing result to an atom var
        _ => (Atom::Var(gensym("tmp")), Some(rco_exp(RVarExpr(e, ty)))),
    }
}

fn rco_op((a, e): (Atom, Option<Expr>), f: impl FnOnce(Atom) -> Expr) -> Expr {
    match (a, e) {
        // void can't be passed to anything
        (Atom::Void, _) => panic!("can't use void as argument"),

        // if 'a' is an atom, just pass it through to f
        (a, None) => f(a),

        // otherwise, use let expression to define new variable
        (Atom::Var(x), Some(e)) => {
            let Expr(a, a_ty) = f(Atom::Var(x.clone()));
            ExprK::Let(x, e.bx(), a.expr(a_ty.clone()).bx()).expr(a_ty)
        }

        // atom can't have expression associated with them
        x @ (Atom::Int(_), Some(_)) => panic!("unsuppoted combo {:?}", x),
        x @ (Atom::Bool(_), Some(_)) => panic!("unsuppoted combo {:?}", x),
    }
}

fn simplify_and_rco_binop(op: RVar::BinaryOpKind, e1: RVarExpr, e2: RVarExpr, ty: Type) -> Expr {
    use RVar::BinaryOpKind as RVarOpKind;
    use RVar::CmpOpKind as RVarCmpKind;

    fn rco_op_apply(op: BinaryOpKind, e1: RVarExpr, e2: RVarExpr, ty: Type) -> Expr {
        rco_op(rco_atom(e1), |x| {
            rco_op(rco_atom(e2), |y| Expr(ExprK::BinaryOp(op, x, y), ty))
        })
    }

    match op {
        // since and & or are shortcicuiting ops, convert to 'if'-expr
        RVarOpKind::And => rco_exp(
            RVarTExpr::If(
                e1.bx(),
                e2.bx(),
                RVarTExpr::Bool(false).texpr(Type::Bool).bx(),
            )
            .texpr(ty),
        ),
        RVarOpKind::Or => rco_exp(
            RVarTExpr::If(
                e1.bx(),
                RVarTExpr::Bool(true).texpr(Type::Bool).bx(),
                e2.bx(),
            )
            .texpr(ty),
        ),
        RVarOpKind::CmpOp(op) => match op {
            RVarCmpKind::Eq => rco_op_apply(BinaryOpKind::Eq, e1, e2, ty),
            RVarCmpKind::Lt => rco_op_apply(BinaryOpKind::Lt, e1, e2, ty),
            RVarCmpKind::Le => unimplemented!(),
            RVarCmpKind::Gt => unimplemented!(),
            RVarCmpKind::Ge => unimplemented!(),
        },
        RVarOpKind::Add => rco_op_apply(BinaryOpKind::Add, e1, e2, ty),
    }
}

fn type_size_in_bytes(ty: &Type) -> Int {
    match ty {
        Type::Bool => 8,
        Type::Int => 8,
        Type::Void => panic!("can't have type void"),
        Type::Tuple(tys) => 8 + tys.iter().map(|ty| type_size_in_bytes(ty)).sum::<Int>(),
    }
}

// remove-complex-opera* {opera* = operations|operands}
pub fn rco_exp(RVarExpr(e, ty): RVarExpr) -> Expr {
    match e {
        RVarTExpr::Int(i) => ExprK::Atom(Atom::Int(i)).expr(ty),
        RVarTExpr::Bool(b) => ExprK::Atom(Atom::Bool(b)).expr(ty),
        RVarTExpr::Var(x) => ExprK::Atom(Atom::Var(x)).expr(ty),
        RVarTExpr::Void => ExprK::Atom(Atom::Void).expr(ty),
        RVarTExpr::Read => ExprK::Read.expr(ty),
        RVarTExpr::BinaryOp(op, e1, e2) => simplify_and_rco_binop(op, *e1, *e2, ty),
        RVarTExpr::UnaryOp(op, expr) => rco_op(rco_atom(*expr), |x| ExprK::UnaryOp(op, x).expr(ty)),
        RVarTExpr::Let(x, e, body) => ExprK::Let(x, rco_exp(*e).bx(), rco_exp(*body).bx()).expr(ty),
        RVarTExpr::If(e1, e2, e3) => {
            ExprK::If(rco_exp(*e1).bx(), rco_exp(*e2).bx(), rco_exp(*e3).bx()).expr(ty)
        }
        RVarTExpr::Tuple(es) => {
            // compute size of the tuple
            let bytes = type_size_in_bytes(&ty);

            // generate a call to the garbage collector
            let collect_expr = expr! {
                (if (lt (add {RVarTExpr::GlobalVar("free_ptr".to_string()).expr()} {bytes})
                    {RVarTExpr::GlobalVar("fromspace_end".to_string()).expr()})
                        {RVarTExpr::Void.expr()}
                            {RVarTExpr::Collect(bytes).expr()})
            };

            // generate a symbol for each tuple element
            let sym = es.iter().map(|_| gensym("tmp")).collect::<Vec<String>>();

            // generate a let of tupleset expr into a tuple allocated on the heap
            let vstr = gensym("tmp");
            let v = RVarTExpr::Var(vstr.clone()).expr();
            let expr = sym
                .iter()
                .cloned()
                .enumerate()
                .rfold(v.clone(), |e, (xidx, xvar)| {
                    expr! {
                    (let [_ (tupleset! {v.clone()} {xidx as Int}
                        {RVarTExpr::Var(xvar).expr()})] {e})}
                });

            // call to allocate tuple on the heap
            let expr = expr! {
                (let [_ {collect_expr}]
                     (let [{vstr} {RVarTExpr::Allocate(1, ty).expr()}] {expr}))
            };

            // bind tuple elements to their respective symbols
            let es = es.into_iter().map(|e| e.untyped());
            let expr = sym.into_iter().zip(es).rfold(expr, |e, (x, xval)| {
                expr! {
                    (let [{x} {xval}] {e})
                }
            });

            /*
             example of generated code for 2-elemen tuple
            let _e1 = expr! {
                (let [x0 (,es[0])]
                 (let [x1 (,es[1])]
                  (let [_ (,collect_expr)]
                   (let [v (,RVarTExpr::Allocate(1,ty).expr())]
                    (let [_ (tupleset! v 0 x0)]
                     (let [_ (tupleset! v 1 x1)] v))))))
            };
            */
            println!("expr= {:?}", expr);
            let ctx: RVar::Ctx = [
                ("free_ptr".to_string(), Type::Int),
                ("fromspace_end".to_string(), Type::Int),
            ]
            .iter()
            .cloned()
            .collect();
            let expr = RVar::typed_expr_impl(&ctx, expr);
            println!("expr= {:?}", expr);

            rco_exp(expr)
        }
        RVarTExpr::TupleRef(_tu, _idx) => {
            //
            unimplemented!()
        }
        RVarTExpr::TupleSet(_tu, _idx, _val) => {
            //
            unimplemented!()
        }
        RVarTExpr::TupleLen(..) => unimplemented!(),
        RVarTExpr::Collect(..) => unimplemented!(),
        RVarTExpr::Allocate(..) => unimplemented!(),
        RVarTExpr::GlobalVar(..) => unimplemented!(),
    }
}

pub fn interp_atom(env: &Env, e: &Atom) -> Value {
    match e {
        Atom::Int(n) => Value::Int(*n),
        Atom::Bool(b) => Value::Bool(*b),
        Atom::Var(x) => sym_get(env, &x).unwrap().clone(),
        Atom::Void => Value::Void,
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
