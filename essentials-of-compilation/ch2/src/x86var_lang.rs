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
pub struct Block(pub Vec<String>, pub Vec<Inst>);
#[derive(Debug, Clone)]
pub struct BlockStack(pub Int, pub Vec<Inst>);

impl Block {
    pub fn empty() -> Block {
        Block(vec![], vec![])
    }
    pub fn with_vars(vars: Vec<String>) -> Block {
        Block(vars, vec![])
    }
}

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

fn interp_arg(frame: &Vec<Value>, env: &Env, arg: &Arg) -> Value {
    match arg {
        Arg::Imm(n) => *n,
        Arg::Reg(reg) => *env_get(env, reg).unwrap(),
        Arg::Deref(Reg::rbp, idx) => frame[(-idx - 8) as usize],
        Arg::Deref(..) => panic!("unimplemented {:?}", arg),
    }
}

pub fn interp_inst(frame: &mut Vec<Value>, env: Env, inst: &Inst) -> Env {
    use Inst::*;

    fn assign(frame: &mut Vec<Value>, env: Env, arg: &Arg, result: Value) -> Env {
        match arg {
            Arg::Reg(reg) => env_set(env, reg.clone(), result),
            Arg::Deref(Reg::rbp, idx) => {
                frame[(-idx - 8) as usize] = result;
                env
            }
            _ => panic!("unhanled {:?}", arg),
        }
    }
    match inst {
        Addq(arg1, arg2) => {
            let result = interp_arg(frame, &env, arg1) + interp_arg(frame, &env, arg2);
            assign(frame, env, arg2, result)
        }
        Movq(arg1, arg2) => {
            let result = interp_arg(frame, &env, arg1);
            assign(frame, env, arg2, result)
        }
        Subq(..) => panic!("unsupported instruction{:?}", inst),
        Negq(arg) => {
            let result = -interp_arg(frame, &env, arg);
            assign(frame, env, arg, result)
        }
        Callq(..) => panic!("unsupported instruction{:?}", inst),
        Pushq(..) => panic!("unsupported instruction{:?}", inst),
        Popq(..) => panic!("unsupported instruction{:?}", inst),
        Jmp(..) => panic!("unsupported instruction{:?}", inst),
        Retq => env,
    }
}
pub fn interp_block(block: &Block) -> Value {
    let Block(_, list) = block;
    let mut env: Env = vec![];
    let mut frame = vec![];
    for inst in list {
        env = interp_inst(&mut frame, env, inst);
    }
    *env_get(&env, &Reg::rax).unwrap()
}

use std::collections::HashMap;
pub fn assign_homes(block: &Block) -> BlockStack {
    let Block(vars, list) = block;
    let mut var2idx: HashMap<String, Int> = HashMap::new();
    let mut stack_size = 0;
    for var in vars {
        assert!(!var2idx.contains_key(var));
        stack_size += 8;
        var2idx.insert(var.clone(), stack_size);
    }

    let home = |arg: &Arg| match arg {
        Arg::Reg(Reg::Var(x)) => {
            let idx = var2idx.get(x).unwrap();
            Arg::Deref(Reg::rbp, -idx)
        }
        _ => arg.clone(),
    };
    let mut list1 = vec![];
    for inst in list {
        use Inst::*;
        let inst1 = match inst {
            Addq(arg1, arg2) => Addq(home(arg1), home(arg2)),
            Subq(arg1, arg2) => Subq(home(arg1), home(arg2)),
            Movq(arg1, arg2) => Movq(home(arg1), home(arg2)),
            Negq(arg) => Negq(home(arg)),
            Retq => Retq,
            Pushq(_) => unimplemented!(),
            Popq(_) => unimplemented!(),
            Jmp(_) => unimplemented!(),
            Callq(_, _) => unimplemented!(),
        };
        list1.push(inst1);
    }
    BlockStack(stack_size, list1)
}

pub fn interp_block_stack(block: &BlockStack) -> Value {
    let BlockStack(stack_size, list) = block;
    let mut env: Env = vec![];
    let mut frame = vec![];
    frame.resize(*stack_size as usize, 0xDEADBEEF);
    for inst in list {
        env = interp_inst(&mut frame, env, inst);
    }
    *env_get(&env, &Reg::rax).unwrap()
}
