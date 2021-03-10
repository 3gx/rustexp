#[path = "./macros.rs"]
mod macros;
//use macros::bx;
use macros::r#match;

#[path = "rvar_lang.rs"]
pub mod rvar_lang;
use rvar_lang::texpr;

use rvar_lang as RVar;
use RVar::Expr as RVarExpr;
use RVar::ExprK as RVarExprK;
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

    TupleRef(Atom, Int),
    TupleSet(Atom, Int, Atom),

    Collect(Int),
    Allocate(Int, Type),
    GlobalVar(String),
}
impl ExprK {
    fn typ(self, typ: Type) -> Expr {
        Expr(self, typ)
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
        RVarExprK::Int(i) => (Atom::Int(i), None),
        RVarExprK::Bool(b) => (Atom::Bool(b), None),
        RVarExprK::Var(x) => (Atom::Var(x), None),
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
            let Expr(_, ty) = &e;
            ExprK::Let(x.clone(), e.bx(), f(Atom::Var(x)).bx()).typ(ty.clone())
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
            rco_op(rco_atom(e2), |y| ExprK::BinaryOp(op, x, y).typ(ty))
        })
    }

    match op {
        // since and & or are shortcicuiting ops, convert to 'if'-expr
        RVarOpKind::And => rco_exp(texpr! { (if {ty} {e1} {e2} false) }),
        RVarOpKind::Or => rco_exp(texpr! { (if {ty} {e1} true {e2}) }),
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

pub fn type_size_in_bytes(ty: &Type) -> Int {
    match ty {
        Type::Bool => 8,
        Type::Int => 8,
        Type::Tuple(tys) => 8 + tys.iter().map(|ty| type_size_in_bytes(ty)).sum::<Int>(),
        Type::Void => unreachable!(),
        Type::Fun(..) => unreachable!(),
    }
}

// remove-complex-opera* {opera* = operations|operands}
pub fn rco_exp(RVarExpr(e, ty): RVarExpr) -> Expr {
    let ty = ty.unwrap();
    match e {
        RVarExprK::Int(i) => ExprK::Atom(Atom::Int(i)).typ(ty),
        RVarExprK::Bool(b) => ExprK::Atom(Atom::Bool(b)).typ(ty),
        RVarExprK::Var(x) => ExprK::Atom(Atom::Var(x)).typ(ty),
        RVarExprK::Void => ExprK::Atom(Atom::Void).typ(ty),
        RVarExprK::Read => ExprK::Read.typ(ty),
        RVarExprK::BinaryOp(op, e1, e2) => simplify_and_rco_binop(op, *e1, *e2, ty),
        RVarExprK::UnaryOp(op, expr) => rco_op(rco_atom(*expr), |x| ExprK::UnaryOp(op, x).typ(ty)),
        RVarExprK::Let(x, e, body) => ExprK::Let(x, rco_exp(*e).bx(), rco_exp(*body).bx()).typ(ty),
        RVarExprK::If(e1, e2, e3) => {
            ExprK::If(rco_exp(*e1).bx(), rco_exp(*e2).bx(), rco_exp(*e3).bx()).typ(ty)
        }
        RVarExprK::Tuple(es) => {
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
                    {RVarExprK::Collect(bytes).typed(Type::Void)})
            };

            // generate a symbol for each tuple element
            let sym = es.iter().map(|_| gensym("tmp")).collect::<Vec<String>>();

            // symbol for tuple expression
            let tusym = gensym("tmp");
            let tuty = ty;
            let tuvar = RVarExprK::Var(tusym.clone()).typed(tuty.clone());

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
                    (let {ety.unwrap().clone()} [_ (tupleset! {tuvar.clone()}
                                                     {xidx as Int}
                                                     {RVarExprK::Var(xvar).typed(xtype)})]
                         {RVarExpr(e,ety.clone())})}
                },
            );

            // call to allocate tuple on the heap
            let RVarExpr(expr, ety) = expr;
            let expr = texpr! {
                (let {ety.unwrap().clone()} [_ {collect_expr}]
                     (let {tuty.clone()}
                          [{tusym} {RVarExprK::Allocate(1, tuty.clone()).typed(tuty.clone())}]
                          {RVarExpr(expr, ety.clone())}))
            };

            // generated a nested bind of tuple elements to their respective symbols
            let expr =
                sym.into_iter()
                    .zip(es.into_iter())
                    .rfold(expr, |RVarExpr(e, ety), (x, xval)| {
                        texpr! {
                            (let {ety.unwrap().clone()} [{x} {xval}] {RVarExpr(e,ety.clone())})
                        }
                    });

            /*
             untyped pseudo-code of a generated code for 2-elemen tuple
            let e = expr! {
                (let [x0 {es[0]}]
                 (let [x1 {es[1]}]
                  (let [_ {collect_expr}]
                   (let [v {RVarExprK::Allocate(1,ty).expr()}]
                    (let [_ (tupleset! v 0 x0)]
                     (let [_ (tupleset! v 1 x1)] v))))))
            };
            */

            rco_exp(expr)
        }
        RVarExprK::TupleRef(tu, idx) => {
            rco_op(rco_atom(*tu), |tu| ExprK::TupleRef(tu, idx).typ(ty))
        }
        RVarExprK::TupleSet(tu, idx, val) => rco_op(rco_atom(*tu), |tu| {
            rco_op(rco_atom(*val), |val| ExprK::TupleSet(tu, idx, val).typ(ty))
        }),
        RVarExprK::Collect(bytes) => ExprK::Collect(bytes).typ(ty),
        RVarExprK::Allocate(num, ty1) => ExprK::Allocate(num, ty1).typ(ty),
        RVarExprK::GlobalVar(x) => ExprK::GlobalVar(x).typ(ty),
        RVarExprK::TupleLen(..) => unimplemented!(),
        RVarExprK::Apply(_fun, _args) => unimplemented!(),
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
            if interp_exp(env, e1).bool().unwrap() {
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
        ExprK::Allocate(1, Type::Tuple(ty)) => {
            let mut val = Vec::new();
            val.resize(ty.len(), Value::default());
            Value::Tuple(val).onheap()
        }
        x @ ExprK::Allocate(..) => panic!("unimplemented {:?}", x),
        ExprK::Collect(bytes) => {
            r#match! { [sym_get(env, "fromspace_end")]
                Some(x) if @{let Value::Heap(x) = x,
                             let Value::Int(end) = &mut *x.borrow_mut()} =>
                         {*end = ((bytes as &Int) + *end)*2;},
                x@_ => panic!("fromspace_end is not valid, got {:?}", x)
            }
            println!("\ncall to collect");
            Value::Void
        }
        ExprK::GlobalVar(var) => {
            r#match! { [sym_get(env, var)]
                Some(x) if @{let Value::Heap(x) = x} => x.borrow().clone(),
                _ => panic!("unknown globalvar {:?}", var)
            }
        }
        ExprK::TupleRef(tu, idx) => {
            r#match! { [interp_atom(env, tu)]
                Value::Heap(el) if @{let Value::Tuple(tu) = &mut *el.borrow_mut()} =>
                    tu[*idx as usize].clone(),
                _ => panic!("expecting tuple, but got  {:?}", tu)
            }
        }
        ExprK::TupleSet(tu, idx, val) => {
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
        ("free_ptr".to_string(), Value::Int(0).onheap()),
        ("fromspace_end".to_string(), Value::Int(0).onheap()),
    ]
    .iter()
    .cloned()
    .collect();
    interp_exp(&env, e)
}
