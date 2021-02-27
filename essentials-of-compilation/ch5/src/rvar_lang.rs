#[path = "./macros.rs"]
mod macros;
use macros::bx;

pub trait IntoTerm {
    fn into_term(&self) -> Expr;
}

macro __mk_op {
    ( (@args) (@expr (@ctor $($ctor:tt)*) $($tt:tt)*) ) => { $($ctor)*($($tt)*) },
    ( (@args false)  (@expr $($tt:tt)*) ) => {
        __mk_op!((@args)
                 (@expr $($tt)* Box::new(Expr::Bool(false))))
    },
    ( (@args true)  (@expr $($tt:tt)*) ) => {
        __mk_op!((@args)
                 (@expr $($tt)* Box::new(Expr::Bool(true))))
    },
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

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum CmpOpKind {
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum UnaryOpKind {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum BinaryOpKind {
    CmpOp(CmpOpKind),
    Add,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    Void,
    Tuple(Vec<Type>),
}

type Int = i64;
type Bool = bool;
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // atoms
    Int(Int),
    Bool(Bool),
    Var(String),

    // let expr
    Let(String, Box<Expr>, Box<Expr>),

    //control-flow
    If(Box<Expr>, Box<Expr>, Box<Expr>),

    Read,

    UnaryOp(UnaryOpKind, Box<Expr>),
    BinaryOp(BinaryOpKind, Box<Expr>, Box<Expr>),

    Tuple(Vec<Expr>),
    TupleLen(Box<Expr>),
    TupleRef(Box<Expr>, Int),
    TupleSet(Box<Expr>, Int, Box<Expr>),
    Void,
    HasType(Box<Expr>, Type),
}

impl Expr {
    pub fn bx(&self) -> Box<Expr> {
        Box::new(self.clone())
    }
}

pub macro int($e:expr) {
    Expr::Int($e)
}
pub macro bool($b:expr) {
    Expr::Bool($b)
}

pub macro r#let {
    ([_ $($expr:tt)*] $($body:tt)*) => {
       __mk_op!((@args $($expr)*, $($body)*)
                (@expr (@ctor Expr::Let) "_".to_string(),) )
    },
    ([$id:ident $($expr:tt)*] $($body:tt)*) => {
       __mk_op!((@args $($expr)*, $($body)*)
                (@expr (@ctor Expr::Let) stringify!($id).to_string(),) )
    },
}
pub use r#let as let_;

pub macro r#if {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*) (@expr (@ctor Expr::If))) }
}
pub use r#if as if_;

pub macro read() {
    Expr::Read
}
pub macro neg {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*) (@expr (@ctor Expr::UnaryOp) UnaryOpKind::Neg,) ) }
}
pub macro not {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*) (@expr (@ctor Expr::UnaryOp) UnaryOpKind::Not,) ) }
}
pub macro add {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*)
                              (@expr (@ctor Expr::BinaryOp)
                                     BinaryOpKind::Add,)) }
}
pub macro eq {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*)
                              (@expr (@ctor Expr::BinaryOp)
                                     BinaryOpKind::CmpOp(CmpOpKind::Eq),) ) }
}
pub macro le {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*)
                              (@expr (@ctor Expr::BinaryOp)
                                     BinaryOpKind::CmpOp(CmpOpKind::Le),) ) }
}
pub macro lt {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*)
                              (@expr (@ctor Expr::BinaryOp)
                                     BinaryOpKind::CmpOp(CmpOpKind::Lt),) ) }
}
pub macro ge {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*)
                              (@expr (@ctor Expr::BinaryOp)
                                     BinaryOpKind::CmpOp(CmpOpKind::Ge),) ) }
}
pub macro gt {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*)
                              (@expr (@ctor Expr::BinaryOp)
                                     BinaryOpKind::CmpOp(CmpOpKind::Gt),) ) }
}
pub macro and {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*)
                              (@expr (@ctor Expr::BinaryOp)
                                     BinaryOpKind::And,) ) }
}
pub macro or {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*)
                              (@expr (@ctor Expr::BinaryOp)
                                     BinaryOpKind::Or,) ) }
}

pub macro tupleref {
    ($tu:ident, $idx:expr) => {Expr::TupleRef(Box::new(stringify!($tu).into_term()),
                                            $idx)},
    ($tu:expr, $idx:expr) => {Expr::TupleRef(tu.into_Term(), $idx)},
}
pub macro tupleset {
    ($tu:ident, $idx:expr, $val:expr) => {
        Expr::TupleSet(Box::new(stringify!($tu).into_term()),
                       $idx,
                       Box::new($val.into_term()))},
    ($tu:ident, $idx:expr, $val:ident) => {
        Expr::TupleSet(Box::new(stringify!($tu).into_term()),
                       $idx,
                       Box::new(stringify!($val).into_term()))},
}
pub macro tuple {
    ($($val:expr),*) => {Expr::Tuple(vec![$($val.into_term()),*])},
}

#[derive(Debug, Clone)]
pub struct Program(pub Expr);

pub macro program($e:expr) {
    Program($e.into_term())
}

impl IntoTerm for Int {
    fn into_term(&self) -> Expr {
        int!(*self)
    }
}
impl IntoTerm for bool {
    fn into_term(&self) -> Expr {
        bool!(*self)
    }
}
impl IntoTerm for Expr {
    fn into_term(&self) -> Expr {
        self.clone()
    }
}
impl IntoTerm for &str {
    fn into_term(&self) -> Expr {
        Expr::Var(self.to_string())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Value {
    Int(Int),
    Bool(Bool),
}
impl Value {
    pub fn int(&self) -> Option<&Int> {
        match self {
            Value::Int(n) => Some(n),
            Value::Bool(_) => None,
        }
    }
    pub fn bool(&self) -> Option<&Bool> {
        match self {
            Value::Int(_) => None,
            Value::Bool(b) => Some(b),
        }
    }
}

type SymTable<T> = Vec<(String, T)>;
pub type Env = SymTable<Value>;

pub macro sym {
    () => {
        Vec::new()
    },
    ($elem:expr; $n:expr) => {
        vec::from_elem($elem, $n)
    },
    ($($x:expr),+ $(,)?) => {
        <[_]>::into_vec(Box::new([$($x),+]))
    },
}

pub fn sym_get<'a, T>(sym: &'a SymTable<T>, key: &str) -> Option<&'a T> {
    sym.iter()
        .rev()
        .find_map(|x| if x.0 != key { None } else { Some(&x.1) })
}

pub fn sym_set<T: Clone>(sym: &SymTable<T>, key: &str, val: &T) -> SymTable<T> {
    let mut sym = sym.clone();
    sym.push((key.to_string(), val.clone()));
    sym
}

pub fn interp_exp(env: &Env, e: &Expr) -> Value {
    use self::CmpOpKind as C;
    use {BinaryOpKind::*, UnaryOpKind::*};
    match e {
        Expr::Int(n) => Value::Int(*n),
        Expr::Bool(b) => Value::Bool(*b),
        Expr::Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            Value::Int(input.trim().parse().unwrap())
        }
        Expr::UnaryOp(Neg, e) => Value::Int(-interp_exp(env, e).int().unwrap()),
        Expr::BinaryOp(Add, e1, e2) => {
            Value::Int(interp_exp(env, e1).int().unwrap() + interp_exp(env, e2).int().unwrap())
        }
        Expr::Var(x) => sym_get(env, &x).unwrap().clone(),
        Expr::Let(x, e, body) => {
            let new_env = sym_set(env, x, &interp_exp(env, e));
            interp_exp(&new_env, body)
        }
        Expr::If(cond, thn, els) => interp_exp(
            env,
            if *interp_exp(env, cond).bool().unwrap() {
                thn
            } else {
                els
            },
        ),
        Expr::BinaryOp(And, e1, e2) => {
            if *interp_exp(env, e1).bool().unwrap() {
                Value::Bool(*interp_exp(env, e2).bool().unwrap())
            } else {
                Value::Bool(false)
            }
        }
        Expr::BinaryOp(Or, e1, e2) => {
            if *interp_exp(env, e1).bool().unwrap() {
                Value::Bool(true)
            } else {
                Value::Bool(*interp_exp(env, e2).bool().unwrap())
            }
        }
        Expr::UnaryOp(Not, expr) => Value::Bool(!interp_exp(env, expr).bool().unwrap()),
        Expr::BinaryOp(BinaryOpKind::CmpOp(op), e1, e2) => {
            match (op, interp_exp(env, e1), interp_exp(env, e2)) {
                (C::Eq, Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
                (C::Eq, Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
                (C::Le, Value::Int(a), Value::Int(b)) => Value::Bool(a <= b),
                (C::Lt, Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                (C::Ge, Value::Int(a), Value::Int(b)) => Value::Bool(a >= b),
                (C::Gt, Value::Int(a), Value::Int(b)) => Value::Bool(a > b),
                x @ _ => panic!("type mismatch: {:?}", x),
            }
        }
        Expr::Tuple(..) => unimplemented!(),
        Expr::TupleLen(..) => unimplemented!(),
        Expr::TupleRef(..) => unimplemented!(),
        Expr::TupleSet(..) => unimplemented!(),
        Expr::Void => unimplemented!(),
        Expr::HasType(..) => unimplemented!(),
    }
}

pub fn interp_program(p: &Program) -> Value {
    match p {
        Program(e) => interp_exp(&vec![], e),
    }
}

use std::collections::HashMap;

use std::cell::RefCell;
thread_local! {
    static MAP: RefCell<HashMap<String, usize>> = RefCell::new(HashMap::new());
}
pub fn gensym_reset() {
    MAP.with(|map| map.borrow_mut().clear());
}
pub fn gensym(x: &str) -> String {
    let counter = MAP.with(|map| {
        let mut map = map.borrow_mut();
        let counter = map.entry(x.to_string()).or_insert(0);
        *counter += 1;
        *counter
    });
    x.to_string() + &counter.to_string()
}

type UMap = SymTable<String>;
pub fn uniquify_expr(umap: &UMap, expr: &Expr) -> Expr {
    use Expr::*;
    match expr {
        Var(x) => Var(sym_get(umap, x).unwrap().clone()),
        Int(n) => Int(*n),
        Bool(b) => Bool(*b),
        Let(x, e, body) => {
            let newvar = gensym(x);
            let umap = sym_set(umap, x, &newvar);
            Let(
                newvar,
                bx![uniquify_expr(&umap, e)],
                bx![uniquify_expr(&umap, body)],
            )
        }
        UnaryOp(op, expr) => UnaryOp(*op, bx![uniquify_expr(umap, expr)]),
        BinaryOp(op, e1, e2) => BinaryOp(
            *op,
            bx![uniquify_expr(umap, e1)],
            bx![uniquify_expr(umap, e2)],
        ),
        Read => Read,
        If(e1, e2, e3) => If(
            bx![uniquify_expr(umap, e1)],
            bx![uniquify_expr(umap, e2)],
            bx![uniquify_expr(umap, e3)],
        ),
        Tuple(..) => unimplemented!(),
        TupleLen(..) => unimplemented!(),
        TupleRef(..) => unimplemented!(),
        TupleSet(..) => unimplemented!(),
        Void => unimplemented!(),
        HasType(..) => unimplemented!(),
    }
}

pub fn uniquify(p: Program) -> Program {
    match p {
        Program(e) => Program(uniquify_expr(&sym![], &e)),
    }
}

pub type Ctx = SymTable<Type>;
pub fn type_expr(ctx: &Ctx, expr: &Expr) -> Type {
    match expr {
        Expr::Int(_) => Type::Int,
        Expr::Bool(_) => Type::Bool,
        Expr::Var(x) => sym_get(ctx, x).unwrap().clone(),
        Expr::Let(x, expr, body) => {
            let ty = type_expr(ctx, expr);
            let ctx = sym_set(ctx, x, &ty);
            type_expr(&ctx, body)
        }
        Expr::If(pred, then_, else_) => {
            match type_expr(ctx, pred) {
                Type::Bool => (),
                x @ _ => panic!("type({:?}) must be Bool, got {:?}", pred, x),
            };
            let then_ty = type_expr(ctx, then_);
            let else_ty = type_expr(ctx, else_);
            if then_ty != else_ty {
                panic!(
                    "type({:?}) = {:?} != {:?} = type({:?})",
                    then_, then_ty, else_ty, else_
                )
            }
            then_ty
        }
        Expr::Read => Type::Int,
        Expr::UnaryOp(op, expr) => match (op, type_expr(ctx, expr)) {
            (UnaryOpKind::Not, Type::Bool) => Type::Bool,
            (UnaryOpKind::Neg, Type::Int) => Type::Int,
            x @ _ => panic!("unsupported {:?}", x),
        },
        Expr::BinaryOp(op, e1, e2) => match (op, type_expr(ctx, e1), type_expr(ctx, e2)) {
            (BinaryOpKind::Add, Type::Int, Type::Int) => Type::Int,
            (BinaryOpKind::And, Type::Bool, Type::Bool) => Type::Bool,
            (BinaryOpKind::Or, Type::Bool, Type::Bool) => Type::Bool,
            (BinaryOpKind::CmpOp(CmpOpKind::Eq), Type::Bool, Type::Bool) => Type::Bool,
            (BinaryOpKind::CmpOp(_), Type::Int, Type::Int) => Type::Bool,
            x @ _ => panic!("unsupported {:?}", x),
        },
        Expr::Tuple(..) => unimplemented!(),
        Expr::TupleLen(..) => unimplemented!(),
        Expr::TupleRef(..) => unimplemented!(),
        Expr::TupleSet(..) => unimplemented!(),
        Expr::Void => unimplemented!(),
        Expr::HasType(..) => unimplemented!(),
    }
}
