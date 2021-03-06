#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Int(i64),
    Read,
    Neg(Box<Term>),
    Add(Box<Term>, Box<Term>),
    Var(String),
    Let(String, Box<Term>, Box<Term>),
}

pub macro add {
    ($e1:ident, $e2:ident) => {
        Term::Add(
            Box::new(var!(stringify!($e1))), Box::new(var!(stringify!($id)))
        )
    },
    ($e1:ident, $e2:expr) => {
        Term::Add(Box::new(var!($e1)), Box::new($e2.into_term()))
    },
    ($e1:expr, $id:ident) => {
        Term::Add(Box::new($e1.into_term()), Box::new(var!(stringify!($id))))
    },
    ($e1:expr, $e2:expr) => {
        Term::Add(Box::new($e1.into_term()), Box::new($e2.into_term()))
    },
}

pub macro neg {
    ($id:ident) => {
        neg!(Box::new(var!($id)))
    },
    ($e:expr) => {
        Term::Neg(Box::new($e.into_term()))
    },
}

pub macro read() {
    Term::Read
}

pub macro int($e:expr) {
    Term::Int($e)
}

pub macro var {
    ($id:ident) => {
        var!(stringify!($id))
    },
    ($id:expr) => {
        Term::Var($id.to_string())
    }
}

pub macro r#let {
    ([$id:ident $e1:expr]  $e2:ident) => {
        Term::Let(
            stringify!($id).to_owned(),
            Box::new($e1.into_term()),
            Box::new(var!(stringify!($e2))),
        )
    },
    ([$id:ident $e1:expr]  $e2:expr) => {
        Term::Let(
            stringify!($id).to_owned(),
            Box::new($e1.into_term()),
            Box::new($e2.into_term()),
        )
    }
}

#[derive(Debug, Clone)]
pub struct Options;

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Options>, pub Term);

pub macro program($e:expr) {
    Program(vec![], $e.into_term())
}

pub trait IntoTerm {
    fn into_term(&self) -> Term;
}

impl IntoTerm for i64 {
    fn into_term(&self) -> Term {
        int!(*self)
    }
}
impl IntoTerm for Term {
    fn into_term(&self) -> Term {
        self.clone()
    }
}
impl IntoTerm for &str {
    fn into_term(&self) -> Term {
        var!(self)
    }
}

type Value = i64;
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

pub fn interp_exp(env: &Env, e: &Term) -> Value {
    use Term::*;
    match e {
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

pub fn interp_program(p: &Program) -> Value {
    match p {
        Program(_, e) => interp_exp(&vec![], e),
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
pub fn uniquify_expr(umap: &UMap, expr: &Term) -> Term {
    use Term::*;
    match expr {
        Var(x) => Var(sym_get(umap, x).unwrap().clone()),
        Int(n) => Int(*n),
        Let(x, e, body) => {
            let newvar = gensym(x);
            let umap = sym_set(umap, x, &newvar);
            Let(
                newvar,
                box uniquify_expr(&umap, e),
                box uniquify_expr(&umap, body),
            )
        }
        Neg(e) => Neg(box uniquify_expr(umap, e)),
        Add(e1, e2) => Add(box uniquify_expr(umap, e1), box uniquify_expr(umap, e2)),
        Read => Read,
    }
}

pub fn uniquify(p: Program) -> Program {
    match p {
        Program(v, e) => Program(v, uniquify_expr(&sym![], &e)),
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn txx() {
        use crate::rvar_lang::*;
        let v = var!("x");
        println!("v= {:?}", v);
    }
}
