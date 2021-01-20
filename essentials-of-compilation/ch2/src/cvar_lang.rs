type Int = i64;
type Label = String;
type Info = Vec<i64>;
type Value = Int;
pub type Env = Vec<(Label, Value)>;

#[path = "macros.rs"]
mod macros;
use macros::r#match;

#[path = "rvar_anf_lang.rs"]
mod rvar_anf_lang;

use rvar_anf_lang as RVarAnf;
pub use rvar_anf_lang::rvar_lang;

pub fn env_get<'a>(env: &'a Env, key: &str) -> Option<&'a Value> {
    env.iter()
        .rev()
        .find_map(|x| if x.0 != key { None } else { Some(&x.1) })
}

pub fn env_set(env: &Env, key: &str, val: Value) -> Env {
    let mut env = env.clone();
    env.push((key.to_string(), val));
    env
}

pub macro env {
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

pub trait IntoAtom {
    type Output;
    fn into_atom(&self) -> Self::Output;
}

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
}

pub macro read() {
    Expr::Read
}

pub macro neg {
    ($id:ident) => {
        neg!(var!($id))
    },
    ($e:expr) => {
        Expr::Neg($e.into_atom())
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

#[derive(Debug, Clone)]
pub enum Stmt {
    AssignVar(String, Expr),
}

#[derive(Debug, Clone)]
pub enum Tail {
    Return(Expr),
    Seq(Stmt, Box<Tail>),
}

#[derive(Debug, Clone)]
pub struct CProgram(pub Info, pub Vec<(Label, Tail)>);

pub fn interp_atom(env: &Env, atom: &Atom) -> Value {
    use Atom::*;
    match atom {
        Int(n) => n.clone(),
        Var(x) => env_get(env, x).unwrap().clone(),
    }
}

pub fn interp_expr(env: &Env, e: &Expr) -> Value {
    use Expr::*;
    match e {
        Atom(atom) => interp_atom(env, atom),
        Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            input.trim().parse().unwrap()
        }
        Neg(atom) => -interp_atom(env, atom),
        Add(a1, a2) => interp_atom(env, a1) + interp_atom(env, a2),
    }
}

pub fn interp_stmt(env: &Env, stmt: &Stmt) -> Env {
    match stmt {
        Stmt::AssignVar(var, exp) => env_set(env, var, interp_expr(env, exp)),
    }
}

pub fn interp_tail(env: &Env, tail: &Tail) -> Value {
    match tail {
        Tail::Return(exp) => interp_expr(env, exp),
        Tail::Seq(stmt, tail) => {
            let new_env = interp_stmt(env, stmt);
            interp_tail(&new_env, tail)
        }
    }
}

pub fn inter_prog(prog: &CProgram) -> Value {
    r#match! { prog,
        CProgram(_, blocks) if @{[(label, tail),..] = &blocks[..],
                                  "start" == label}
                            => interp_tail(&env![], tail),
        _ => panic!("unhandled {:?}", prog),
    }
}

fn from_atom(a: &RVarAnf::Atom) -> Atom {
    match a {
        RVarAnf::Atom::Int(n) => Atom::Int(*n),
        RVarAnf::Atom::Var(v) => Atom::Var(v.clone()),
    }
}

pub fn explicate_tail(e: &RVarAnf::Expr) -> (Tail, Vec<String>) {
    match e {
        RVarAnf::Expr::Atom(a) => (Tail::Return(Expr::Atom(from_atom(a))), vec![]),
        RVarAnf::Expr::Read => (Tail::Return(Expr::Read), vec![]),
        RVarAnf::Expr::Neg(a) => (Tail::Return(Expr::Neg(from_atom(a))), vec![]),
        RVarAnf::Expr::Add(a1, a2) => (
            Tail::Return(Expr::Add(from_atom(a1), from_atom(a2))),
            vec![],
        ),
        RVarAnf::Expr::Let(x, e_not_tail, e_tail) => {
            let (tail, vars1) = explicate_tail(e_tail);
            let (tail, vars2) = explicate_assign(e_not_tail, x, &tail);
            let mut vars = vec![x.clone()];
            for v in vars1.iter() {
                vars.push(v.clone());
            }
            for v in vars2.iter() {
                vars.push(v.clone());
            }
            (tail, vars)
        }
    }
}

pub fn explicate_assign(e: &RVarAnf::Expr, var: &str, tail: &Tail) -> (Tail, Vec<String>) {
    let assign = |e: Expr| Tail::Seq(Stmt::AssignVar(var.to_string(), e), Box::new(tail.clone()));
    match e {
        RVarAnf::Expr::Atom(a) => (assign(Expr::Atom(from_atom(a))), vec![]),
        RVarAnf::Expr::Read => (assign(Expr::Read), vec![]),
        RVarAnf::Expr::Neg(a) => (assign(Expr::Neg(from_atom(a))), vec![]),
        RVarAnf::Expr::Add(a1, a2) => (assign(Expr::Add(from_atom(a1), from_atom(a2))), vec![]),
        RVarAnf::Expr::Let(x, e1, e2) => {
            let (tail, vars1) = explicate_assign(e1, x, tail);
            let (tail, vars2) = explicate_assign(e2, var, &tail);
            let mut vars = vec![x.clone()];
            for v in vars1.iter() {
                vars.push(v.clone());
            }
            for v in vars2.iter() {
                vars.push(v.clone());
            }
            (tail, vars)
        }
    }
}
