type Int = i64;
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(Int),
    Read,
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Var(String),
    Let(String, Box<Expr>, Box<Expr>),
}

#[path = "./macros.rs"]
mod macros;
pub use macros::__mk_op;
use macros::{bx, r#match, IntoExpr};

pub macro add {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*) (@expr (@ctor Expr::Add))
                              (@vctor Expr::Var))},
}

pub macro neg {
    ($($tt:tt)*) => {__mk_op!((@args $($tt)*) (@expr (@ctor Expr::Neg))
                              (@vctor Expr::Var))},
}

pub macro read() {
    Expr::Read
}

pub macro int($e:expr) {
    Expr::Int($e)
}

pub macro r#let {
    ([$id:ident $($expr:tt)*] $($body:tt)*) => {
       __mk_op!((@args $($expr)*, $($body)*)
                (@expr (@ctor Expr::Let) stringify!($id).to_string(),)
                (@vctor Expr::Var))
    },
}

#[derive(Debug, Clone)]
pub struct Program(pub Expr);

pub macro program($e:expr) {
    Program($e.into_expr())
}

impl IntoExpr<Expr> for Int {
    fn into_expr(&self) -> Expr {
        int!(*self)
    }
}
impl IntoExpr<Expr> for Expr {
    fn into_expr(&self) -> Expr {
        self.clone()
    }
}
impl IntoExpr<Expr> for &str {
    fn into_expr(&self) -> Expr {
        Expr::Var(self.to_string())
    }
}

type SymTable<T> = Vec<(String, T)>;
pub type Env = SymTable<Int>;

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

pub fn interp_exp(env: &Env, e: &Expr) -> Int {
    use Expr::*;
    r#match! { [e]
        Int(n) => n.clone(),
        Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            input.trim().parse().unwrap()
        }
        Neg(e) => -interp_exp(env, e),
        Add(e1, e2) => interp_exp(env, e1) + interp_exp(env, e2),
        Var(x) => sym_get(env, &x).unwrap().clone(),
        Let(x, e, body) => {
            let new_env = sym_set(env, x, &interp_exp(env, e));
            interp_exp(&new_env, body)
        }
    }
}

pub fn interp_program(p: &Program) -> Int {
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
fn gensym(x: &String) -> String {
    let mut map = MAP.lock().unwrap();
    let counter = map.entry(x.to_string()).or_insert(0);
    *counter += 1;

    x.clone() + &counter.to_string()
}

type UMap = SymTable<String>;
pub fn uniquify_expr(umap: &UMap, expr: &Expr) -> Expr {
    use Expr::*;
    match expr {
        Var(x) => Var(sym_get(umap, x).unwrap().clone()),
        Int(n) => Int(*n),
        Let(x, e, body) => {
            let newvar = gensym(x);
            let umap = sym_set(umap, x, &newvar);
            Let(
                newvar,
                bx![uniquify_expr(&umap, e)],
                bx![uniquify_expr(&umap, body)],
            )
        }
        Neg(e) => Neg(bx![uniquify_expr(umap, e)]),
        Add(e1, e2) => Add(bx![uniquify_expr(umap, e1)], bx![uniquify_expr(umap, e2)]),
        Read => Read,
    }
}

pub fn uniquify(p: Program) -> Program {
    match p {
        Program(e) => Program(uniquify_expr(&sym![], &e)),
    }
}
