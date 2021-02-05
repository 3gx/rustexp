#[path = "./macros.rs"]
mod macros;
use macros::bx;

pub trait IntoTerm {
    fn into_term(&self) -> Expr;
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

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CmpOpKind {
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum UnaryOpKind {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum BinaryOpKind {
    CmpOp(CmpOpKind),
    Add,
    And,
    Or,
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
    }
}

pub fn interp_program(p: &Program) -> Value {
    match p {
        Program(e) => interp_exp(&vec![], e),
    }
}

lazy_static! {
    static ref MAP: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
}
use lazy_static::lazy_static; // 1.4.0
use std::collections::HashMap;
use std::sync::Mutex;

pub fn gensym_reset() {
    let mut map = MAP.lock().unwrap();
    map.clear();
}
pub fn gensym(x: &str) -> String {
    let mut map = MAP.lock().unwrap();
    let counter = map.entry(x.to_string()).or_insert(0);
    *counter += 1;

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
    }
}

pub fn uniquify(p: Program) -> Program {
    match p {
        Program(e) => Program(uniquify_expr(&sym![], &e)),
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Type {
    Bool,
    Int,
}

pub type Ctx = SymTable<Type>;
pub fn type_check(ctx: &Ctx, e: &Expr) -> Result<Type, Vec<String>> {
    unimplemented!()
}
