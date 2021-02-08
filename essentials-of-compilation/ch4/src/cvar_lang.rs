#[path = "./macros.rs"]
mod macros;
use macros::r#match;

#[path = "rvar_anf_lang.rs"]
pub mod rvar_anf_lang;
pub use rvar_anf_lang as RVarAnf;
pub use RVarAnf::rvar_lang as RVar;

pub use RVarAnf::{int, var, Atom, BinaryOpKind, Bool, Int, UnaryOpKind};
use RVarAnf::{interp_atom, Value};
use RVarAnf::{sym_get, sym_set, Env};

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(Atom),
    Read,
    UnaryOp(UnaryOpKind, Atom),
    BinaryOp(BinaryOpKind, Atom, Atom),
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

pub fn interp_expr(env: &Env, e: &Expr) -> Value {
    use Expr::*;
    match e {
        Atom(atom) => interp_atom(env, atom),
        Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            Value::Int(input.trim().parse().unwrap())
        }
        UnaryOp(op, a) => match (op, interp_atom(env, a)) {
            (UnaryOpKind::Not, Value::Bool(b)) => Value::Bool(!b),
            (UnaryOpKind::Neg, Value::Int(i)) => Value::Int(-i),
            x @ _ => panic!("type mismatch: {:?}", x),
        },
        BinaryOp(op, a1, a2) => match (op, interp_atom(env, a1), interp_atom(env, a2)) {
            (BinaryOpKind::Add, Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (BinaryOpKind::Eq, Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
            (BinaryOpKind::Eq, Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
            (BinaryOpKind::Lt, Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
            x @ _ => panic!("type mismatch: {:?}", x),
        },
    }
}

pub fn interp_stmt(env: &Env, stmt: &Stmt) -> Env {
    match stmt {
        Stmt::AssignVar(var, exp) => sym_set(env, var, &interp_expr(env, exp)),
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

pub fn interp_prog(prog: &CProgram) -> Value {
    r#match! { prog,
        CProgram(_, blocks) if @{[(label, tail),..] = &blocks[..],
                                  "start" == label}
                            => interp_tail(&vec![], tail),
        _ => panic!("unhandled {:?}", prog),
    }
}

pub fn explicate_impl(e: &RVarAnf::Expr, var_n_tail: Option<(&str, &Tail)>) -> (Tail, Vec<String>) {
    let mk_tail = |e: Expr| {
        (
            match var_n_tail {
                Some((var, tail)) => {
                    Tail::Seq(Stmt::AssignVar(var.to_string(), e), Box::new(tail.clone()))
                }
                None => Tail::Return(e),
            },
            vec![],
        )
    };
    match e {
        RVarAnf::Expr::Atom(a) => mk_tail(Expr::Atom(a.clone())),
        RVarAnf::Expr::Read => mk_tail(Expr::Read),
        RVarAnf::Expr::UnaryOp(op, a) => mk_tail(Expr::UnaryOp(*op, a.clone())),
        RVarAnf::Expr::BinaryOp(op, a1, a2) => mk_tail(Expr::BinaryOp(*op, a1.clone(), a2.clone())),
        RVarAnf::Expr::Let(x, expr, body) => {
            let (tail, vars1) = explicate_impl(body, var_n_tail);
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
        _ => panic!("unhandled expression: {:?}", e),
    }
}

pub fn explicate_tail(e: &RVarAnf::Expr) -> (Tail, Vec<String>) {
    explicate_impl(e, None)
}
/*
pub fn explicate_assign(e: &RVarAnf::Expr, var: &str, tail: &Tail) -> (Tail, Vec<String>) {
    explicate_impl(e, Some((var, tail)))
}
*/
