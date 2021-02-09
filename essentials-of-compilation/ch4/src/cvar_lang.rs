#[path = "./macros.rs"]
mod macros;
use macros::r#match;

#[path = "rvar_anf_lang.rs"]
pub mod rvar_anf_lang;
pub use rvar_anf_lang as RVarAnf;
pub use RVarAnf::rvar_lang as RVar;

use RVar::{gensym, gensym_reset, sym_get, sym_set, Env};
pub use RVarAnf::{int, var, Atom, BinaryOpKind, Bool, Int, UnaryOpKind};
use RVarAnf::{interp_atom, Value};

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

#[derive(Debug, Clone, Copy)]
pub enum CmpOp {
    Eq,
    Lt,
}

#[derive(Debug, Clone)]
pub enum Tail {
    Return(Expr),
    Seq(Stmt, Box<Tail>),
    Goto(String),
    IfStmt(Expr, String, String),
}

#[derive(Debug, Clone)]
pub struct BasicBlock(pub String, pub Tail);

#[derive(Debug, Clone)]
pub struct CProgram(pub Vec<BasicBlock>);

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
        Tail::Goto(label) => unimplemented!(),
        Tail::IfStmt(compare, thn, els) => unimplemented!(),
    }
}

pub fn interp_prog(prog: &CProgram) -> Value {
    r#match! { prog,
        CProgram(blocks) if @{[BasicBlock(label, tail),..] = &blocks[..],
                                  "start" == label}
                            => interp_tail(&vec![], tail),
        _ => panic!("unhandled {:?}", prog),
    }
}

fn explicate_if(
    e: &RVarAnf::Expr,
    then_name: &str,
    else_name: &str,
    bbs: Vec<BasicBlock>,
) -> (Tail, Vec<BasicBlock>) {
    match e {
        RVarAnf::Expr::BinaryOp(cmp, a1, a2)
            if *cmp == BinaryOpKind::Eq || *cmp == BinaryOpKind::Lt =>
        {
            (
                Tail::IfStmt(
                    Expr::BinaryOp(*cmp, a1.clone(), a2.clone()),
                    then_name.to_string(),
                    else_name.to_string(),
                ),
                bbs,
            )
        }
        RVarAnf::Expr::Atom(Atom::Bool(pred)) => (
            Tail::IfStmt(
                Expr::Atom(Atom::Bool(*pred)),
                then_name.to_string(),
                else_name.to_string(),
            ),
            bbs,
        ),
        RVarAnf::Expr::Atom(Atom::Var(var)) => (
            Tail::IfStmt(
                Expr::Atom(Atom::Var(var.clone())),
                then_name.to_string(),
                else_name.to_string(),
            ),
            bbs,
        ),
        RVarAnf::Expr::If(p_expr, t_expr, e_expr) => {
            let (then_tail, bbs) = explicate_if(t_expr, then_name, else_name, bbs);
            let (else_tail, mut bbs) = explicate_if(e_expr, then_name, else_name, bbs);
            let then_name = gensym("then_bb");
            let else_name = gensym("else_bb");
            bbs.push(BasicBlock(then_name.clone(), then_tail));
            bbs.push(BasicBlock(else_name.clone(), else_tail));
            explicate_if(p_expr, &then_name, &else_name, bbs)
        }
        x @ _ => panic!("invalid if predicate= {:?}", x),
    }
}

fn explicate_tail(e: &RVarAnf::Expr, bbs: Vec<BasicBlock>) -> (Tail, Vec<BasicBlock>) {
    match e {
        RVarAnf::Expr::Atom(a) => (Tail::Return(Expr::Atom(a.clone())), bbs),
        RVarAnf::Expr::Read => (Tail::Return(Expr::Read), bbs),
        RVarAnf::Expr::UnaryOp(op, a) => (Tail::Return(Expr::UnaryOp(*op, a.clone())), bbs),
        RVarAnf::Expr::BinaryOp(op, a1, a2) => (
            Tail::Return(Expr::BinaryOp(*op, a1.clone(), a2.clone())),
            bbs,
        ),
        RVarAnf::Expr::Let(x, expr, body) => {
            let (tail, bbs) = explicate_tail(body, bbs);
            explicate_assign(expr, x, tail, bbs)
        }
        RVarAnf::Expr::If(cnd, thn, els) => {
            let (then_bb, bbs) = explicate_tail(thn, bbs);
            let (else_bb, mut bbs) = explicate_tail(els, bbs);
            let then_name = gensym("then_bb");
            let else_name = gensym("else_bb");
            bbs.push(BasicBlock(then_name.clone(), then_bb));
            bbs.push(BasicBlock(else_name.clone(), else_bb));
            explicate_if(cnd, &then_name, &else_name, bbs)
        }
    }
}

fn explicate_assign(
    e: &RVarAnf::Expr,
    var: &str,
    tail: Tail,
    bbs: Vec<BasicBlock>,
) -> (Tail, Vec<BasicBlock>) {
    let assign = |e: Expr| Stmt::AssignVar(var.to_string(), e);
    match e {
        RVarAnf::Expr::Atom(a) => (
            Tail::Seq(assign(Expr::Atom(a.clone())), Box::new(tail)),
            bbs,
        ),
        RVarAnf::Expr::Read => (Tail::Seq(assign(Expr::Read), Box::new(tail)), bbs),
        RVarAnf::Expr::UnaryOp(op, a) => (
            Tail::Seq(assign(Expr::UnaryOp(*op, a.clone())), Box::new(tail)),
            bbs,
        ),
        RVarAnf::Expr::BinaryOp(op, a1, a2) => (
            Tail::Seq(
                assign(Expr::BinaryOp(*op, a1.clone(), a2.clone())),
                Box::new(tail),
            ),
            bbs,
        ),
        RVarAnf::Expr::Let(x, expr, body) => {
            let (tail, bbs) = explicate_assign(body, var, tail, bbs);
            explicate_assign(expr, x, tail, bbs)
        }
        RVarAnf::Expr::If(cnd, thn, els) => {
            unimplemented!()
        }
    }
}

pub fn explicate_expr(e: &RVarAnf::Expr) -> CProgram {
    let (tail, mut bbs) = explicate_tail(e, vec![]);
    bbs.push(BasicBlock("start".to_string(), tail));
    CProgram(bbs)
}

/*
pub fn explicate_assign(e: &RVarAnf::Expr, var: &str, tail: &Tail) -> (Tail, Vec<String>) {
    explicate_impl(e, Some((var, tail)))
}
*/
