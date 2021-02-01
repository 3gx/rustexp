#[path = "./macros.rs"]
mod macros;
#[allow(unused_imports)]
use macros::r#match;

#[path = "cvar_lang.rs"]
pub mod cvar_lang;
pub use cvar_lang::rvar_anf_lang;
pub use rvar_anf_lang::rvar_lang;

type Int = i64;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
}

#[derive(Debug, Clone)]
pub enum Arg {
    Imm(Int),
    Var(String),
    Reg(Reg),
    Deref(Reg, Int),
}

#[derive(Debug, Clone)]
pub enum Inst {
    Addq(Arg, Arg),
    Subq(Arg, Arg),
    Movq(Arg, Arg),
    Negq(Arg),
    Callq(String, Int /*arity*/),
    Retq,
    Pushq(Arg),
    Popq(Arg),
    Jmp(String),
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

use cvar_lang as CVarLang;
pub fn select_inst_atom(a: &CVarLang::Atom) -> Arg {
    match a {
        CVarLang::Atom::Int(n) => Arg::Imm(*n),
        CVarLang::Atom::Var(x) => Arg::Var(x.clone()),
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
        Stmt::AssignVar(x, e) => select_inst_assign(Arg::Var(x.clone()), e),
    }
}

pub fn select_inst_tail(t: &CVarLang::Tail, block: Block) -> Block {
    use CVarLang::Tail;
    use Reg::*;

    match t {
        Tail::Return(expr) => {
            let Block(info, mut list) = block;
            for inst in select_inst_assign(Arg::Reg(rax), expr) {
                list.push(inst)
            }
            //list.push(Retq);
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnvKey {
    Reg(Reg),
    Var(String),
}

pub type Env = Vec<(EnvKey, Int)>;
fn env_get<'a>(env: &'a Env, reg: &EnvKey) -> Option<&'a Int> {
    env.iter()
        .rev()
        .find_map(|x| if &x.0 != reg { None } else { Some(&x.1) })
}

fn env_set(mut env: Env, reg: EnvKey, val: Int) -> Env {
    env.push((reg, val));
    env
}

fn interp_arg(frame: &Vec<Int>, env: &Env, arg: &Arg) -> Int {
    match arg {
        Arg::Imm(n) => *n,
        Arg::Reg(reg) => *env_get(env, &EnvKey::Reg(*reg)).unwrap(),
        Arg::Var(x) => *env_get(env, &EnvKey::Var(x.clone())).unwrap(),
        Arg::Deref(Reg::rbp, idx) => frame[(-idx - 8) as usize],
        Arg::Deref(..) => panic!("unimplemented {:?}", arg),
    }
}

pub fn interp_inst(frame: &mut Vec<Int>, env: Env, inst: &Inst) -> Env {
    use Inst::*;

    fn assign(frame: &mut Vec<Int>, env: Env, arg: &Arg, result: Int) -> Env {
        match arg.clone() {
            Arg::Reg(reg) => env_set(env, EnvKey::Reg(reg), result),
            Arg::Var(x) => env_set(env, EnvKey::Var(x), result),
            Arg::Deref(Reg::rbp, idx) => {
                frame[(-idx - 8) as usize] = result;
                env
            }
            x @ Arg::Deref(_, _) => panic!("cannot assign to no rbp loc {:?}", x),
            x @ Arg::Imm(_) => panic!("cannot assignt to immediate {:?}", x),
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
pub fn interp_block(block: &Block) -> Int {
    let Block(_, list) = block;
    let mut env: Env = vec![];
    let mut frame = vec![];
    for inst in list {
        env = interp_inst(&mut frame, env, inst);
    }
    *env_get(&env, &EnvKey::Reg(Reg::rax)).unwrap()
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
        Arg::Var(x) => {
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

pub fn interp_block_stack(block: &BlockStack) -> Int {
    let BlockStack(stack_size, list) = block;
    let mut env: Env = vec![];
    let mut frame = vec![];
    frame.resize(*stack_size as usize, 0xDEADBEEF);
    for inst in list {
        env = interp_inst(&mut frame, env, inst);
    }
    *env_get(&env, &EnvKey::Reg(Reg::rax)).unwrap()
}

pub fn patch_x86(block: &BlockStack) -> BlockStack {
    let BlockStack(stack_size, list) = block;
    let mut list1 = vec![];
    for inst in list {
        use Inst::*;
        use Reg::{rax, rbp};
        let insts1 = match inst {
            Addq(Arg::Deref(rbp, idx1), Arg::Deref(rbp, idx2)) => vec![
                Movq(Arg::Deref(rbp, *idx1), Arg::Reg(rax)),
                Addq(Arg::Reg(rax), Arg::Deref(rbp, *idx2)),
            ],
            Subq(Arg::Deref(rbp, idx1), Arg::Deref(rbp, idx2)) => vec![
                Movq(Arg::Deref(rbp, *idx1), Arg::Reg(rax)),
                Subq(Arg::Reg(rax), Arg::Deref(rbp, *idx2)),
            ],
            Movq(Arg::Deref(rbp, idx1), Arg::Deref(rbp, idx2)) => vec![
                Movq(Arg::Deref(rbp, *idx1), Arg::Reg(rax)),
                Movq(Arg::Reg(rax), Arg::Deref(rbp, *idx2)),
            ],
            _ => vec![inst.clone()],
        };
        for inst in insts1 {
            list1.push(inst)
        }
    }
    BlockStack(*stack_size, list1)
}

fn print_x86arg(arg: &Arg) -> String {
    match arg {
        Arg::Imm(n) => format!("${}", n),
        x @ Arg::Var(_) => panic!("Can't have Arg::Var in final x86 {:?}", x),
        Arg::Reg(reg) => format!("%{:?}", reg),
        Arg::Deref(reg, idx) => format!("{}(%{:?})", idx, reg),
    }
}
fn print_x86inst(inst: &Inst) -> String {
    use Inst::*;
    match inst {
        Addq(arg1, arg2) => format!("addq\t{}, {}", print_x86arg(arg1), print_x86arg(arg2)),
        Movq(arg1, arg2) => format!("movq\t{}, {}", print_x86arg(arg1), print_x86arg(arg2)),
        Negq(arg) => format!("negq\t{}", print_x86arg(arg)),
        //Retq => format!("retq"),
        _ => panic!("unhanded {:?}", inst),
    }
}
pub fn print_x86(block: &BlockStack) -> String {
    let BlockStack(stack_size, list) = block;
    let mut x86block = vec![];
    for inst in list {
        x86block.push(print_x86inst(inst))
    }

    let mut prog = String::new();
    prog.push_str("start:\n");
    for inst in x86block {
        prog.push_str(&("\t".to_string() + &inst + "\n"));
    }
    prog.push_str("\tjmp\tconclusion\n");
    prog.push_str("\n");
    prog.push_str("\t.globl _main\n");
    prog.push_str("_main:\n");
    prog.push_str("\tpush %rbp\n");
    prog.push_str("\tmovq\t%rsp,%rbp\n");
    prog.push_str(format!("\tsubq\t${},%rsp\n", stack_size).as_str());
    prog.push_str("\tjmp start\n");
    prog.push_str("\n");
    prog.push_str("conclusion:\n");
    prog.push_str(format!("\taddq\t ${}, %rsp\n", stack_size).as_str());
    prog.push_str("\tpopq\t%rbp\n");
    prog.push_str("\tretq\n");
    prog
}

// ---------------------------------------------------------------------------
// liveness analysis
// interference graph
// graph coloring
// patch instructions
// print x86

use std::collections::HashSet;
pub fn liveness_analysis(block: &Block) -> Vec<HashSet<String>> {
    fn update_with_rdwr(live_set: &mut HashSet<String>, rd: &[&Arg], wr: &[&Arg]) {
        // before_k = (after_k - wr) + rd;
        for arg in wr {
            match arg {
                Arg::Var(x) => live_set.remove(x),
                _ => false,
            };
        }
        for arg in rd {
            match arg {
                Arg::Var(x) => live_set.insert(x.clone()),
                _ => false,
            };
        }
    }
    let update_live_set = |inst: &Inst, live_set: &HashSet<String>| {
        let mut live_set = live_set.clone();
        use Inst::*;
        match inst {
            Addq(a1, a2) => update_with_rdwr(&mut live_set, &[a1, a2], &[a2]),
            Subq(a1, a2) => update_with_rdwr(&mut live_set, &[a1, a2], &[a2]),
            Movq(a1, a2) => update_with_rdwr(&mut live_set, &[a1], &[a2]),
            Negq(arg) => update_with_rdwr(&mut live_set, &[arg], &[arg]),
            Callq(_, _) => unimplemented!(),
            Retq => (),
            Pushq(_) => unimplemented!(),
            Popq(_) => unimplemented!(),
            Jmp(_) => unimplemented!(),
        };
        live_set
    };

    let Block(_, list) = block;
    let mut live_set_vec: Vec<HashSet<String>> = vec![HashSet::new()];
    for inst in list.iter().rev() {
        let live_set = update_live_set(inst, live_set_vec.last().unwrap());
        live_set_vec.push(live_set);
    }
    // reverse vector
    live_set_vec.reverse();
    // pop last element, which is empty set
    live_set_vec.pop();
    live_set_vec
}
