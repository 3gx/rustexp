#[path = "./macros.rs"]
mod macros;
use macros::r#match;

#[path = "rvar_anf_lang.rs"]
pub mod rvar_anf_lang;
pub use rvar_anf_lang::rvar_lang;

type Int = i64;

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

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(Atom),
    Read,
    Neg(Atom),
    Add(Atom, Atom),
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
pub struct CProgram(pub Vec<String>, pub Vec<(String, Tail)>);

use rvar_lang::{sym_get, sym_set, EnvInt};

pub fn interp_atom(env: &EnvInt, atom: &Atom) -> Int {
    use Atom::*;
    match atom {
        Int(n) => n.clone(),
        Var(x) => sym_get(env, x).unwrap().clone(),
    }
}

pub fn interp_expr(env: &EnvInt, e: &Expr) -> Int {
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

pub fn interp_stmt(env: &EnvInt, stmt: &Stmt) -> EnvInt {
    match stmt {
        Stmt::AssignVar(var, exp) => sym_set(env, var, &interp_expr(env, exp)),
    }
}

pub fn interp_tail(env: &EnvInt, tail: &Tail) -> Int {
    match tail {
        Tail::Return(exp) => interp_expr(env, exp),
        Tail::Seq(stmt, tail) => {
            let new_env = interp_stmt(env, stmt);
            interp_tail(&new_env, tail)
        }
    }
}

pub fn interp_prog(prog: &CProgram) -> Int {
    r#match! { prog,
        CProgram(_, blocks) if @{[(label, tail),..] = &blocks[..],
                                  "start" == label}
                            => interp_tail(&vec![], tail),
        _ => panic!("unhandled {:?}", prog),
    }
}

use rvar_anf_lang as RVarAnf;
pub fn explicate_impl(e: &RVarAnf::Expr, old_tail: Option<(&str, &Tail)>) -> (Tail, Vec<String>) {
    fn from_atom(a: &RVarAnf::Atom) -> Atom {
        match a {
            RVarAnf::Atom::Int(n) => Atom::Int(*n),
            RVarAnf::Atom::Var(v) => Atom::Var(v.clone()),
        }
    }
    let mk_tail = |e: Expr| {
        (
            match old_tail {
                Some((var, tail)) => {
                    Tail::Seq(Stmt::AssignVar(var.to_string(), e), Box::new(tail.clone()))
                }
                None => Tail::Return(e),
            },
            vec![],
        )
    };
    match e {
        RVarAnf::Expr::Atom(a) => mk_tail(Expr::Atom(from_atom(a))),
        RVarAnf::Expr::Read => mk_tail(Expr::Read),
        RVarAnf::Expr::Neg(a) => mk_tail(Expr::Neg(from_atom(a))),
        RVarAnf::Expr::Add(a1, a2) => mk_tail(Expr::Add(from_atom(a1), from_atom(a2))),
        RVarAnf::Expr::Let(x, expr, body) => {
            let (tail, vars1) = explicate_impl(body, old_tail);
            let (tail, vars2) = explicate_impl(expr, Some((x, &tail)));
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

pub fn explicate_tail(e: &RVarAnf::Expr) -> (Tail, Vec<String>) {
    explicate_impl(e, None)
}
pub fn explicate_assign(e: &RVarAnf::Expr, var: &str, tail: &Tail) -> (Tail, Vec<String>) {
    explicate_impl(e, Some((var, tail)))
}
