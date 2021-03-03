#[path = "./macros.rs"]
mod macros;
//use macros::bx;
use macros::r#match;

#[path = "rvar_lang.rs"]
pub mod rvar_lang;
use rvar_lang::texpr;

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
pub enum Expr {
    Atom(Atom),

    Let(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),

    Read,
    UnaryOp(UnaryOpKind, Atom),
    BinaryOp(BinaryOpKind, Atom, Atom),

    TupleRef(Atom, Int),
    TupleSet(Atom, Int, Atom),

    Collect(Int),
    Allocate(Int, Type),
    GlobalVar(String),
}

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
        (Atom::Var(x), Some(e)) => Expr::Let(x.clone(), e.bx(), f(Atom::Var(x)).bx()),

        // atom can't have expression associated with them
        x @ (Atom::Int(_), Some(_)) => panic!("unsuppoted combo {:?}", x),
        x @ (Atom::Bool(_), Some(_)) => panic!("unsuppoted combo {:?}", x),
    }
}

fn simplify_and_rco_binop(op: RVar::BinaryOpKind, e1: RVarExpr, e2: RVarExpr, ty: Type) -> Expr {
    use RVar::BinaryOpKind as RVarOpKind;
    use RVar::CmpOpKind as RVarCmpKind;

    fn rco_op_apply(op: BinaryOpKind, e1: RVarExpr, e2: RVarExpr) -> Expr {
        rco_op(rco_atom(e1), |x| {
            rco_op(rco_atom(e2), |y| Expr::BinaryOp(op, x, y))
        })
    }

    match op {
        // since and & or are shortcicuiting ops, convert to 'if'-expr
        RVarOpKind::And => rco_exp(texpr! { (if {ty} {e1} {e2} false) }),
        RVarOpKind::Or => rco_exp(texpr! { (if {ty} {e1} true {e2}) }),
        /*
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
            */
        RVarOpKind::CmpOp(op) => match op {
            RVarCmpKind::Eq => rco_op_apply(BinaryOpKind::Eq, e1, e2),
            RVarCmpKind::Lt => rco_op_apply(BinaryOpKind::Lt, e1, e2),
            RVarCmpKind::Le => unimplemented!(),
            RVarCmpKind::Gt => unimplemented!(),
            RVarCmpKind::Ge => unimplemented!(),
        },
        RVarOpKind::Add => rco_op_apply(BinaryOpKind::Add, e1, e2),
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
        RVarTExpr::Int(i) => Expr::Atom(Atom::Int(i)),
        RVarTExpr::Bool(b) => Expr::Atom(Atom::Bool(b)),
        RVarTExpr::Var(x) => Expr::Atom(Atom::Var(x)),
        RVarTExpr::Void => Expr::Atom(Atom::Void),
        RVarTExpr::Read => Expr::Read,
        RVarTExpr::BinaryOp(op, e1, e2) => simplify_and_rco_binop(op, *e1, *e2, ty),
        RVarTExpr::UnaryOp(op, expr) => rco_op(rco_atom(*expr), |x| Expr::UnaryOp(op, x)),
        RVarTExpr::Let(x, e, body) => Expr::Let(x, rco_exp(*e).bx(), rco_exp(*body).bx()),
        RVarTExpr::If(e1, e2, e3) => {
            Expr::If(rco_exp(*e1).bx(), rco_exp(*e2).bx(), rco_exp(*e3).bx())
        }
        RVarTExpr::Tuple(es) => {
            // compute size of the tuple
            let bytes = type_size_in_bytes(&ty);

            // generate a call to the garbage collector
            let collect_expr = texpr! {
                (if {Type::Void}
                    (lt {Type::Bool}
                        (add {Type::Int}
                             (gvar {Type::Int} "free_ptr")
                             (int {bytes}))
                        (gvar {Type::Int} "fromspace_end"))
                    void
                    {RVarTExpr::Collect(bytes).texpr(Type::Void)})
            };

            // generate a symbol for each tuple element
            let sym = es.iter().map(|_| gensym("tmp")).collect::<Vec<String>>();

            // symbol for tuple expression
            let tusym = gensym("tmp");
            let tuty = ty;
            let tuvar = RVarTExpr::Var(tusym.clone()).texpr(tuty.clone());

            // get tuple element types
            let elty = match &tuty {
                Type::Tuple(elty) => elty.clone(),
                _ => panic!("not a tuple type {:?}", tuty),
            };

            // generate a nested let of tupleset! for each elmt into a tuple allocated on the heap
            let expr = sym.iter().cloned().zip(elty.into_iter()).enumerate().rfold(
                tuvar.clone(),
                |RVarExpr(e, ety), (xidx, (xvar, xtype))| {
                    texpr! {
                    (let {ety.clone()} [_ (tupleset! {tuvar.clone()}
                                                     {xidx as Int}
                                                     {RVarTExpr::Var(xvar).texpr(xtype)})]
                         {RVarExpr(e,ety.clone())})}
                },
            );

            // call to allocate tuple on the heap
            let RVarExpr(expr, ety) = expr;
            let expr = texpr! {
                (let {ety.clone()} [_ {collect_expr}]
                     (let {tuty.clone()}
                          [{tusym} {RVarTExpr::Allocate(1, tuty.clone()).texpr(tuty.clone())}]
                          {RVarExpr(expr, ety.clone())}))
            };

            // generated a nested bind of tuple elements to their respective symbols
            let expr =
                sym.into_iter()
                    .zip(es.into_iter())
                    .rfold(expr, |RVarExpr(e, ety), (x, xval)| {
                        texpr! {
                            (let {ety.clone()} [{x} {xval}] {RVarExpr(e,ety.clone())})
                        }
                    });

            /*
             untyped pseudo-code of a generated code for 2-elemen tuple
            let e = expr! {
                (let [x0 {es[0]}]
                 (let [x1 {es[1]}]
                  (let [_ {collect_expr}]
                   (let [v {RVarTExpr::Allocate(1,ty).expr()}]
                    (let [_ (tupleset! v 0 x0)]
                     (let [_ (tupleset! v 1 x1)] v))))))
            };
            */

            rco_exp(expr)
        }
        RVarTExpr::TupleRef(tu, idx) => rco_op(rco_atom(*tu), |tu| Expr::TupleRef(tu, idx)),
        RVarTExpr::TupleSet(tu, idx, val) => rco_op(rco_atom(*tu), |tu| {
            rco_op(rco_atom(*val), |val| Expr::TupleSet(tu, idx, val))
        }),
        RVarTExpr::Collect(bytes) => Expr::Collect(bytes),
        RVarTExpr::Allocate(num, ty1) => Expr::Allocate(num, ty1),
        RVarTExpr::GlobalVar(x) => Expr::GlobalVar(x),
        RVarTExpr::TupleLen(..) => unimplemented!(),
    }
}

pub fn interp_atom(env: &Env, e: &Atom) -> Value {
    match e {
        Atom::Int(n) => Value::Int(*n),
        Atom::Bool(b) => Value::Bool(*b),
        Atom::Var(x) => match sym_get(env, &x) {
            Some(x) => x.clone(),
            _ => panic!("varible {:?} is not defined", x),
        },
        Atom::Void => Value::Void,
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
            if interp_exp(env, e1).bool().unwrap() {
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
        Expr::Allocate(1, Type::Tuple(ty)) => {
            let mut val = Vec::new();
            val.resize(ty.len(), Value::default());
            Value::Tuple(val).onheap()
        }
        x @ Expr::Allocate(..) => panic!("unimplemented {:?}", x),
        Expr::Collect(_bytes) => Value::Void,
        Expr::GlobalVar(label) => interp_atom(env, &Atom::Var(label.clone())),
        Expr::TupleRef(tu, idx) => {
            r#match! { [interp_atom(env, tu)]
                Value::Heap(el) if @{let Value::Tuple(tu) = &mut *el.borrow_mut()} =>
                    tu[*idx as usize].clone(),
                _ => panic!("expecting tuple, but got  {:?}", tu)
            }
        }
        Expr::TupleSet(tu, idx, val) => {
            r#match! { [(interp_atom(env, tu), interp_atom(env, val))]
                (Value::Heap(el), val) if @{let Value::Tuple(tu) = &mut *el.borrow_mut()} =>
                    tu[*idx as usize] = val,
                _ => panic!("expecting tuple, but got  {:?}", tu)
            };
            Value::Void
        }
    }
}

pub fn interp_expr(e: &Expr) -> Value {
    let env: Env = [
        ("free_ptr".to_string(), Value::Int(0)),
        ("fromspace_end".to_string(), Value::Int(0)),
    ]
    .iter()
    .cloned()
    .collect();
    interp_exp(&env, e)
}
