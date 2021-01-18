type Int = i64;
type Label = String;
type Info = Vec<i64>;
type Value = Int;

#[derive(Debug, Clone)]
pub enum Atom {
    Int(Int),
    Var(String),
}
pub macro int($e:expr) {
    Atom::Int($e)
}
pub macro var {
    ($id:ident) => {
        var!(stringify!($id))
    },
    ($id:expr) => {
        Atom::Var($id.to_string())
    }
}
pub trait IntoAtom {
    type Output;
    fn into_atom(&self) -> Self::Output;
}

impl IntoAtom for i64 {
    type Output = Atom;
    fn into_atom(&self) -> Atom {
        int!(*self)
    }
}
impl IntoAtom for &str {
    type Output = Atom;
    fn into_atom(&self) -> Atom {
        var!(self)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(Atom),
    Read,
    Neg(Atom),
    Add(Atom, Atom),
    Let(String, Box<Expr>, Box<Expr>),
}

pub macro read() {
    Expr::Read
}

pub macro neg {
    ($id:ident) => {
        neg!(var!($id))
    },
    ($e:expr) => {
        Expr(Neg, $e.into_atom())
    },
}

pub macro add {
    ($i1:ident, $i2:ident) => {
        Expr::Add(var!(stringify!($i1)), var!($i2))
    },
    ($i1:ident, $a2:expr) => {
        Expr::Add(var!($i1), $a2.into_atom())
    },
    ($a1:expr, $i2:ident) => {
        Expr::Add($a1.into_atom(), var!($i2)),
    },
    ($a1:expr, $a2:expr) => {
        Expr::Add($a1.into_atom(), $a2.into_atom())
    },
}

#[path = "cvar_lang.rs"]
mod cvar_lang;
#[path = "./macros.rs"]
mod macros;
#[path = "rvar_lang.rs"]
mod rvar_lang;

use macros::r#match;

#[derive(Debug, Clone)]
pub struct Program(Info, Expr);

use rvar_lang::Term as RVarTerm;

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
fn gensym(x: &str) -> String {
    let mut map = MAP.lock().unwrap();
    let counter = map.entry(x.to_string()).or_insert(0);
    *counter += 1;

    x.to_string() + &counter.to_string()
}

pub fn rco_exp(e: &RVarTerm) -> Expr {
    fn is_atom(e: &RVarTerm) -> bool {
        match e {
            RVarTerm::Int(_) | RVarTerm::Var(_) => true,
            _ => false,
        }
    }
    r#match! { [e]
        RVarTerm::Int(i) => Expr::Atom(int!(*i)),
        RVarTerm::Var(x) => Expr::Atom(var!(&x)),
        RVarTerm::Read => Expr::Read,
        RVarTerm::Add(e1, e2) => match (is_atom(e1),is_atom(e2)) {
            (true,true) => Expr::Add(rco_atom(e1), rco_atom(e2)),
            (false,true) => {
                let t1 = gensym("tmp");
                Expr::Let(t1.clone(), box rco_exp(e1), box Expr::Add(var!(&t1), rco_atom(e2)))
            },
            (true,false) => {
                let t2 = gensym("tmp");
                Expr::Let(t2.clone(), box rco_exp(e2), box Expr::Add(rco_atom(e1), var!(&t2)))
            },
            (false, false) => {
                let t1 = gensym("tmp");
                let t2 = gensym("tmp");
                Expr::Let(t1.clone(), box rco_exp(e1),
                    box Expr::Let(t2.clone(), box rco_exp(e2), box Expr::Add(var!(&t1), var!(&t2))))
            },
        },
        RVarTerm::Neg(e) if is_atom(e) => Expr::Neg(rco_atom(e)),
        RVarTerm::Neg(e) => {
            let t = gensym("tmp");
            Expr::Let(t.clone(), box rco_exp(e), box Expr::Neg(var!(&t)))
        },
        RVarTerm::Let(x, e, body) => Expr::Let(x.clone(), box rco_exp(e), box rco_exp(body)),
    }
}
pub fn rco_atom(e: &RVarTerm) -> Atom {
    r#match! { [e]
        RVarTerm::Int(i) => int!(*i),
        RVarTerm::Var(x) => var!(&x),
        _ => panic!("not an atomic expression {:?}", e)
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
        <[_]>::into_vec(box [$($x),+])
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

pub fn interp_atom(env: &Env, e: &Atom) -> Value {
    match e {
        Atom::Int(n) => *n,
        Atom::Var(x) => sym_get(env, &x).unwrap().clone(),
    }
}
pub fn interp_exp(env: &Env, e: &Expr) -> Value {
    match e {
        Expr::Atom(atom) => interp_atom(env, atom),
        Expr::Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            input.trim().parse().unwrap()
        }
        Expr::Neg(e) => -interp_atom(env, e),
        Expr::Add(e1, e2) => interp_atom(env, e1) + interp_atom(env, e2),
        Expr::Let(x, e, body) => {
            let new_env = sym_set(env, x, &interp_exp(env, e));
            interp_exp(&new_env, body)
        }
    }
}

#[cfg(test)]
mod rvar_anf_lang {
    #[test]
    fn t1() {
        use super::{interp_exp, rco_exp};
        let (p1, v1) = {
            use super::rvar_lang::*;
            let p1 = r#let!([x add!(12, add!(neg!(20), neg!(add!(10,neg!(15)))))]
                    add!(add!(30, neg!(15)), x));
            let v1 = interp_exp(&vec![], &p1);
            (p1, v1)
        };
        println!("p1= {:#?} ", p1);

        let p1anf = rco_exp(&p1);
        println!("p1= {:#?} ", p1anf);

        let v1anf = interp_exp(&vec![], &p1anf);

        println!("v1anf= {}", v1anf);
        assert_eq!(v1, v1anf);
    }

    #[test]
    fn t2() {
        use super::rco_exp;
        let p1 = {
            use super::rvar_lang::*;
            r#let!([a 42] r#let!([b var!(a)] b))
        };
        println!("p1= {:#?} ", p1);

        let p1anf = rco_exp(&p1);
        println!("p1= {:#?} ", p1anf);
    }

    #[test]
    fn cvar1() {
        use super::cvar_lang;
        use super::cvar_lang::*;
        let v = int!(42);
        println!("v= {:?}", v);
        let v = var!(x);
        println!("v= {:?}", v);

        let exp = Exp::Prim(add!(10, 32));
        println!("expr= {:?}", exp);
        println!("res= {}", interp_exp(&cvar_lang::env![], &exp));
    }

    #[test]
    fn ast1() {
        use super::macros::r#match;
        #[derive(Debug, Clone)]
        enum OpCode {
            Neg,
            Add,
        }
        #[derive(Debug, Clone)]
        enum Expr {
            Int(i64),
            Prim(OpCode, Vec<Expr>),
        }
        use Expr::*;
        use OpCode::*;

        let e0 = Int(32);
        let e1 = Prim(Neg, vec![Int(32)]);
        let e1a = Prim(Neg, vec![Prim(Add, vec![Int(10), Int(32)])]);
        let e2 = Prim(
            Add,
            vec![Prim(Add, vec![Int(32), Prim(Neg, vec![Int(44)])]), Int(42)],
        );
        println!("e0= {:?}", e0);
        println!("e1= {:?}", e1);
        println!("e1a= {:?}", e1a);
        println!("e2= {:?}", e2);

        fn eval(e: &Expr) -> i64 {
            r#match! { [e]
                Int(n) => *n,
                Prim(Neg, v) if @{let [Int(m)] = v.as_slice()} => -*m,
                Prim(Neg, v) if @{let [e] = &v[..]} => -eval(e),
                Prim(Add, v) if @{let [e1,e2] = &v[..]} => eval(e1) + eval(e2),
                _ => panic!("unhandled {:?}", e)
            }
        }
        println!("eval(e0)= {}", eval(&e0));
        println!("eval(e1)= {}", eval(&e1));
        println!("eval(e1a)= {}", eval(&e1a));
        println!("eval(e2)= {}", eval(&e2));
    }
}
