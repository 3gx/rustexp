#[path = "macros.rs"]
mod macros;
#[allow(unused_imports)]
use macros::r#match;

#[path = "cvar_lang.rs"]
pub mod cvar_lang;
pub use cvar_lang::rvar_anf_lang::rvar_lang;

use cvar_lang as CVarLang;

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum Reg {
    rsp,
    rbp,
    rax,
    rbx,
    rcx,
    rdx,
    rsi,
    rdi,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
    Var(String),
}

type Int = i64;
#[derive(Debug, Clone)]
pub enum Arg {
    Imm(Int),
    Reg(Reg),
    Deref(Reg, Int),
}

type Label = String;
#[derive(Debug, Clone)]
pub enum Inst {
    Addq(Arg, Arg),
    Subq(Arg, Arg),
    Movq(Arg, Arg),
    Negq(Arg),
    Callq(Label, Int /*arity*/),
    Retq,
    Pushq(Arg),
    Popq(Arg),
    Jmp(Label),
}

#[derive(Debug, Clone)]
pub struct Block(Vec<String>, Vec<Inst>);

pub fn select_inst_atom(a: &CVarLang::Atom) -> Arg {
    match a {
        CVarLang::Atom::Int(n) => Arg::Imm(*n),
        CVarLang::Atom::Var(x) => Arg::Reg(Reg::Var(x.clone())),
    }
}

pub fn select_inst_assign(dst: Arg, e: &CVarLang::Expr) -> Vec<Inst> {
    use CVarLang::Expr;
    use Inst::*;
    use Reg::*;
    match e {
        Expr::Atom(a) => vec![Movq(select_inst_atom(a), dst)],
        Expr::Read => vec![Callq("Read".to_string(), 0), Movq(Arg::Reg(rax), dst)],
        Expr::Neg(a) => vec![Movq(select_inst_atom(a), dst.clone()), Negq(dst)],
        Expr::Add(a1, a2) => vec![
            Movq(select_inst_atom(a2), dst.clone()),
            Addq(select_inst_atom(a1), dst),
        ],
    }
}
pub fn select_inst_stmt(s: &CVarLang::Stmt) -> Vec<Inst> {
    use CVarLang::Stmt;
    match s {
        Stmt::AssignVar(x, e) => select_inst_assign(Arg::Reg(Reg::Var(x.clone())), e),
    }
}

pub fn select_inst_tail(t: &CVarLang::Tail, block: Block) -> Block {
    use CVarLang::Tail;
    use {Inst::*, Reg::*};

    match t {
        Tail::Return(expr) => {
            let Block(info, mut list) = block;
            for inst in select_inst_assign(Arg::Reg(rax), expr) {
                list.push(inst)
            }
            list.push(Retq);
            Block(info, list)
        }
        Tail::Seq(stmt, tail) => {
            let Block(info, mut list) = block;
            for inst in select_inst_stmt(stmt) {
                list.push(inst);
            }
            select_inst_tail(tail, Block(info, list))
        }
    }
}

type Value = Int;
pub type Env = Vec<(Reg, Int)>;
fn env_get<'a>(env: &'a Env, reg: &Reg) -> Option<&'a Value> {
    env.iter()
        .rev()
        .find_map(|x| if &x.0 != reg { None } else { Some(&x.1) })
}

fn env_set(mut env: Env, reg: Reg, val: Value) -> Env {
    env.push((reg, val));
    env
}

fn interp_arg(env: &Env, arg: &Arg) -> Value {
    unimplemented!()
}

pub fn interp_inst(env: Env, inst: &Inst) -> Env {
    use Inst::*;
    use Reg::*;
    fn get_reg(arg: &Arg) -> &Reg {
        match arg {
            Arg::Reg(reg) => reg,
            _ => panic!("must be reg"),
        }
    }
    match inst {
        Addq(arg1, arg2) => {
            let result = interp_arg(&env, arg1) + interp_arg(&env, arg2);
            env_set(env, get_reg(arg2).clone(), result)
        }
        _ => unimplemented!(),
    }
}
pub fn interp_block(block: &Block) -> Value {
    let Block(_, list) = block;
    let mut env: Env = vec![];
    for inst in list {
        env = interp_inst(env, inst);
    }
    42
}
