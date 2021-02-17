#[path = "./macros.rs"]
mod macros;
#[allow(unused_imports)]
use macros::r#match;

#[path = "cvar_lang.rs"]
pub mod cvar_lang;
pub use cvar_lang as CVar;
pub use CVar::rvar_anf_lang as RVarAnf;
pub use RVarAnf::rvar_lang as RVarLang;
pub use RVarAnf::Value;

type Int = i64;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[allow(non_camel_case_types)]
pub enum Reg {
    rsp,
    rbp,
    rax,
    rbx, // 0
    rcx, // 1
    rdx, // 2
    rsi, // 3
    rdi, // 4
    r8,  // 5
    r9,  // 6
    r10, // 7
    r11, // 8
    r12, // 9
    r13, // 10
    r14, // 11
    r15, // 12
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[allow(non_camel_case_types)]
pub enum ByteReg {
    ah,
    al,
    bh,
    bl,
    ch,
    cl,
    dh,
    dl,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    Imm(Int),
    Var(String),
    Reg(Reg),
    ByteReg(ByteReg),
    Deref(Reg, Int),
    EFlag, // 0: eq, 1:lt, 2:gt
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryKind {
    Addq,
    Subq,
    Movq,
    Xorq,
    Cmpq,
    Movzbq,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryKind {
    Negq,
    Pushq,
    Popq,
    Set(CndCode),
}

#[derive(Debug, Clone, Copy)]
pub enum CndCode {
    Eq,
    Lt,
}

#[derive(Debug, Clone)]
pub enum Inst {
    Unary(UnaryKind, Arg),
    Binary(BinaryKind, Arg, Arg),
    Callq(String, Int /*arity*/),
    Retq,
    Jmp(String),
    JmpIf(CndCode, String),
}

#[derive(Debug, Clone)]
pub struct BlockVarOpts {
    pub vars: BTreeSet<String>,
    pub regs: BTreeMap<String, Reg>,
}
#[derive(Debug, Clone)]
pub struct BlockVar(pub BlockVarOpts, pub Vec<Inst>);
#[derive(Debug, Clone)]
pub struct BlockStack(pub Int, pub Vec<Inst>);

#[derive(Debug, Clone)]
pub struct BasicBlock(pub String, pub BTreeSet<String>, pub Vec<Inst>);

#[derive(Debug, Clone)]
pub struct Options {
    pub stack: usize,
    pub vars: BTreeSet<String>,
    pub regs: BTreeMap<String, Reg>,
}
#[derive(Debug, Clone)]
pub struct Program(pub Options, pub Vec<BasicBlock>);

impl BlockVar {
    pub fn new() -> BlockVar {
        BlockVar(
            BlockVarOpts {
                vars: BTreeSet::new(),
                regs: BTreeMap::new(),
            },
            vec![],
        )
    }
    pub fn with_vars(mut self, vars: Vec<String>) -> BlockVar {
        for v in vars {
            self.0.vars.insert(v);
        }
        self
    }
    pub fn with_regs(mut self, regs: BTreeMap<String, Reg>) -> BlockVar {
        self.0.regs = regs;
        self
    }
    pub fn with_inst(mut self, inst: Vec<Inst>) -> BlockVar {
        self.1 = inst;
        self
    }
}

pub fn select_inst_atom(a: &CVar::Atom) -> Arg {
    match a {
        CVar::Atom::Int(n) => Arg::Imm(*n),
        CVar::Atom::Bool(b) => {
            if *b {
                Arg::Imm(1)
            } else {
                Arg::Imm(0)
            }
        }
        CVar::Atom::Var(x) => Arg::Var(x.clone()),
    }
}

pub fn select_inst_assign(dst: Arg, e: &CVar::Expr) -> Vec<Inst> {
    use BinaryKind::*;
    use CVar::Expr;
    use CVar::{BinaryOpKind, UnaryOpKind};
    use Inst::*;
    use Reg::*;
    use UnaryKind::*;
    match e {
        Expr::Atom(a) => vec![Binary(Movq, select_inst_atom(a), dst)],
        Expr::Read => vec![
            Callq("Read".to_string(), 0),
            Binary(Movq, Arg::Reg(rax), dst),
        ],
        Expr::UnaryOp(UnaryOpKind::Neg, a) => {
            vec![
                Binary(Movq, select_inst_atom(a), dst.clone()),
                Unary(Negq, dst),
            ]
        }
        Expr::UnaryOp(UnaryOpKind::Not, a) => {
            vec![
                Binary(Movq, select_inst_atom(a), dst.clone()),
                Binary(Xorq, Arg::Imm(1), dst),
            ]
        }
        Expr::BinaryOp(BinaryOpKind::Add, a1, a2) => vec![
            Binary(Movq, select_inst_atom(a1), dst.clone()),
            Binary(Addq, select_inst_atom(a2), dst),
        ],
        Expr::BinaryOp(BinaryOpKind::Eq, a1, a2) => {
            vec![
                Binary(Cmpq, select_inst_atom(a2), select_inst_atom(a1)),
                Unary(Set(CndCode::Eq), Arg::ByteReg(ByteReg::al)),
                Binary(Movzbq, Arg::ByteReg(ByteReg::al), dst),
            ]
        }
        x @ _ => panic!("unhandled expression {:?}", x),
    }
}

pub fn select_inst_stmt(s: &CVar::Stmt) -> Vec<Inst> {
    use CVar::Stmt;
    match s {
        Stmt::AssignVar(x, e) => select_inst_assign(Arg::Var(x.clone()), e),
    }
}

fn get_vars(inst: &Inst) -> BTreeSet<String> {
    use Inst::*;
    let mut vars = BTreeSet::new();
    let mut add_var = |arg: &Arg| match arg {
        Arg::Var(x) => {
            vars.insert(x.clone());
        }
        _ => (),
    };
    match inst {
        Binary(_, arg1, arg2) => {
            add_var(arg1);
            add_var(arg2);
        }
        Unary(_, arg) => {
            add_var(arg);
        }
        Callq(..) => (),
        Jmp(..) => (),
        JmpIf(..) => (),
        Retq => (),
    };
    vars
}
pub fn select_inst_tail(t: &CVar::Tail, block: BlockVar) -> BlockVar {
    use CVar::Tail;
    use Reg::*;

    match t {
        Tail::Return(expr) => {
            let BlockVar(mut info, mut list) = block;
            for inst in select_inst_assign(Arg::Reg(rax), expr) {
                for v in get_vars(&inst) {
                    info.vars.insert(v);
                }
                list.push(inst);
            }
            BlockVar(info, list)
        }
        Tail::Seq(stmt, tail) => {
            let BlockVar(mut info, mut list) = block;
            for inst in select_inst_stmt(stmt) {
                for v in get_vars(&inst) {
                    info.vars.insert(v);
                }
                list.push(inst);
            }
            select_inst_tail(tail, BlockVar(info, list))
        }
        Tail::Goto(label) => {
            let BlockVar(info, mut list) = block;
            list.push(Inst::Jmp(label.clone()));
            BlockVar(info, list)
        }
        Tail::IfStmt(expr, thn, els) => match expr {
            CVar::Expr::BinaryOp(cmpop, a1, a2) => {
                let a1 = select_inst_atom(a1);
                let a2 = select_inst_atom(a2);
                let mut insts = Vec::new();
                let cond = match cmpop {
                    CVar::BinaryOpKind::Eq => CndCode::Eq,
                    CVar::BinaryOpKind::Lt => CndCode::Lt,
                    x @ _ => panic!("unhandled 'if' binary predicate {:?}", x),
                };
                insts.push(Inst::Binary(BinaryKind::Cmpq, a2, a1));
                insts.push(Inst::JmpIf(cond, thn.clone()));
                insts.push(Inst::Jmp(els.clone()));
                let BlockVar(mut info, mut list) = block;
                for inst in insts {
                    for v in get_vars(&inst) {
                        info.vars.insert(v);
                    }
                    list.push(inst);
                }
                BlockVar(info, list)
            }
            CVar::Expr::UnaryOp(CVar::UnaryOpKind::Not, a) => {
                let a1 = select_inst_atom(a);
                let a2 = select_inst_atom(&CVar::Atom::Int(1));
                let a3 = select_inst_atom(&CVar::Atom::Int(0));
                let mut insts = Vec::new();
                insts.push(Inst::Binary(BinaryKind::Xorq, a2.clone(), a1.clone()));
                insts.push(Inst::Binary(BinaryKind::Cmpq, a3, a1));
                insts.push(Inst::JmpIf(CndCode::Eq, els.clone()));
                insts.push(Inst::Jmp(thn.clone()));
                let BlockVar(mut info, mut list) = block;
                for inst in insts {
                    for v in get_vars(&inst) {
                        info.vars.insert(v);
                    }
                    list.push(inst);
                }
                BlockVar(info, list)
            }
            CVar::Expr::Atom(atom @ CVar::Atom::Var(_)) => {
                let a1 = select_inst_atom(atom);
                let a2 = select_inst_atom(&CVar::Atom::Int(0));
                let mut insts = Vec::new();
                insts.push(Inst::Binary(BinaryKind::Cmpq, a2, a1));
                insts.push(Inst::JmpIf(CndCode::Eq, els.clone()));
                insts.push(Inst::Jmp(thn.clone()));
                let BlockVar(mut info, mut list) = block;
                for inst in insts {
                    for v in get_vars(&inst) {
                        info.vars.insert(v);
                    }
                    list.push(inst);
                }
                BlockVar(info, list)
            }
            x @ _ => panic!("unhandled 'if' predicate {:?}", x),
        },
    }
}
pub fn select_inst_prog(cprog: CVar::CProgram) -> Program {
    let CVar::CProgram(bbs) = cprog;
    let mut x86bbs = Vec::new();
    let mut all_vars = BTreeSet::new();
    for CVar::BasicBlock(name, tail) in bbs {
        let BlockVar(BlockVarOpts { vars, regs }, insts) = select_inst_tail(&tail, BlockVar::new());
        assert_eq!(regs.len(), 0);
        for var in &vars {
            all_vars.insert(var.clone());
        }
        x86bbs.push(BasicBlock(name, vars, insts))
    }
    Program(
        Options {
            stack: 0,
            vars: all_vars,
            regs: BTreeMap::new(),
        },
        x86bbs,
    )
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnvKey {
    Reg(Reg),
    ByteReg(ByteReg),
    Var(String),
    EFlag, // 0: eq, 1: lt, 2: gt
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
        Arg::ByteReg(breg) => *env_get(env, &EnvKey::ByteReg(*breg)).unwrap(),
        Arg::Var(x) => *env_get(env, &EnvKey::Var(x.clone())).unwrap(),
        Arg::EFlag => *env_get(env, &EnvKey::EFlag).unwrap(),
        Arg::Deref(Reg::rbp, idx) => frame[(-idx - 8) as usize],
        Arg::Deref(..) => panic!("unimplemented {:?}", arg),
    }
}

pub fn interp_inst(
    frame: &mut Vec<Int>,
    env: Env,
    mut insts: Vec<Inst>,
    prog: &BTreeMap<String, Vec<Inst>>,
) -> Env {
    use Inst::*;

    fn assign(frame: &mut Vec<Int>, env: Env, arg: &Arg, result: Int) -> Option<Env> {
        let env = match arg.clone() {
            Arg::Reg(reg) => env_set(env, EnvKey::Reg(reg), result),
            Arg::ByteReg(breg) => env_set(env, EnvKey::ByteReg(breg), result),
            Arg::Var(x) => env_set(env, EnvKey::Var(x), result),
            Arg::EFlag => env_set(env, EnvKey::EFlag, result),
            Arg::Deref(Reg::rbp, idx) => {
                frame[(-idx - 8) as usize] = result;
                env
            }
            x @ Arg::Deref(_, _) => panic!("cannot assign to no rbp loc {:?}", x),
            x @ Arg::Imm(_) => panic!("cannot assignt to immediate {:?}", x),
        };
        Some(env)
    }
    let inst = insts.pop();
    if inst.is_none() {
        return env;
    }
    let inst = &inst.unwrap();

    match inst {
        Binary(op, arg1, arg2) => match op {
            BinaryKind::Addq => {
                let result = interp_arg(frame, &env, arg1) + interp_arg(frame, &env, arg2);
                let env = assign(frame, env, arg2, result).unwrap();
                interp_inst(frame, env, insts, prog)
            }
            BinaryKind::Xorq => {
                let result = interp_arg(frame, &env, arg1) ^ interp_arg(frame, &env, arg2);
                let env = assign(frame, env, arg2, result).unwrap();
                interp_inst(frame, env, insts, prog)
            }
            BinaryKind::Movq | BinaryKind::Movzbq => {
                let result = interp_arg(frame, &env, arg1);
                let env = assign(frame, env, arg2, result).unwrap();
                interp_inst(frame, env, insts, prog)
            }
            /*
            BinaryKind::Movzbq => {
                let result = interp_arg(frame, &env, arg1);
                assign(frame, env, arg2, result)
            }
            */
            BinaryKind::Cmpq => {
                let arg1 = interp_arg(frame, &env, arg1);
                let arg2 = interp_arg(frame, &env, arg2);
                let eflag = if arg1 == arg2 {
                    0
                } else if arg1 < arg2 {
                    1
                } else {
                    2
                };
                let env = assign(frame, env, &Arg::EFlag, eflag).unwrap();
                interp_inst(frame, env, insts, prog)
            }
            BinaryKind::Subq => panic!("unsupported instruction{:?}", inst),
        },
        Unary(op, arg) => match op {
            UnaryKind::Negq => {
                let result = -interp_arg(frame, &env, arg);
                let env = assign(frame, env, arg, result).unwrap();
                interp_inst(frame, env, insts, prog)
            }
            UnaryKind::Pushq => panic!("unsupported instruction{:?}", inst),
            UnaryKind::Popq => panic!("unsupported instruction{:?}", inst),
            UnaryKind::Set(cc) => {
                let eflag = interp_arg(frame, &env, &Arg::EFlag);
                let eflag = match cc {
                    CndCode::Lt => eflag == 1,
                    CndCode::Eq => eflag == 0,
                };
                let env = assign(frame, env, arg, eflag as Int).unwrap();
                interp_inst(frame, env, insts, prog)
            }
        },
        Jmp(label) => interp_inst(
            frame,
            env,
            prog.get(label).unwrap().iter().rev().cloned().collect(),
            prog,
        ),

        JmpIf(cc, label) => {
            let eflag = interp_arg(frame, &env, &Arg::EFlag);
            let do_jmp = match cc {
                CndCode::Lt => eflag == 1,
                CndCode::Eq => eflag == 0,
            };
            if do_jmp {
                interp_inst(
                    frame,
                    env,
                    prog.get(label).unwrap().iter().rev().cloned().collect(),
                    prog,
                )
            } else {
                interp_inst(frame, env, insts, prog)
            }
        }
        Retq => panic!("unsupported instructin: {:?}", inst),
        Callq(..) => panic!("unsupported instruction: {:?}", inst),
    }
}

/*
fn interp_bb(
    frame: &mut Vec<Int>,
    mut env: Env,
    insts: Vec<Inst>,
    prog: &BTreeMap<String, Vec<Inst>>,
) -> Env {
    let insts: Vec<Inst> = insts.into_iter().rev().collect();
    for inst in &insts {
        env = interp_inst(frame, env, inst, prog);
    }
    env
}
*/

pub fn interp_block(block: &BlockVar) -> Int {
    let BlockVar(_, list) = block;
    let mut env: Env = vec![];
    let mut frame = vec![];
    env = interp_inst(
        &mut frame,
        env,
        list.iter().rev().cloned().collect(),
        &BTreeMap::new(),
    );
    *env_get(&env, &EnvKey::Reg(Reg::rax)).unwrap()
}

pub fn interp_prog(prog: &Program) -> Value {
    let Program(_, bbs) = prog;
    let mut prog = BTreeMap::new();
    for BasicBlock(name, _, insts) in bbs.clone() {
        prog.insert(name, insts);
    }
    let insts = prog.get(&"start".to_string()).unwrap();
    let mut env: Env = vec![];
    let mut frame = vec![];
    env = interp_inst(
        &mut frame,
        env,
        insts.iter().rev().cloned().collect(),
        &prog,
    );
    Value::Int(*env_get(&env, &EnvKey::Reg(Reg::rax)).unwrap())
}

pub fn assign_homes(block: &BlockVar) -> BlockStack {
    let BlockVar(BlockVarOpts { vars, regs }, list) = block;
    let mut var2idx: HashMap<String, Int> = HashMap::new();
    let mut stack_size = 0;
    for var in vars {
        assert!(!var2idx.contains_key(var));
        if regs.get(var).is_none() {
            stack_size += 8;
            var2idx.insert(var.clone(), stack_size);
        }
    }

    let home = |arg: &Arg| match arg {
        Arg::Var(x) => {
            if let Some(reg) = regs.get(x) {
                Arg::Reg(*reg)
            } else {
                let idx = var2idx.get(x).unwrap();
                Arg::Deref(Reg::rbp, -idx)
            }
        }
        _ => arg.clone(),
    };
    let mut list1 = vec![];
    for inst in list {
        use Inst::*;
        let inst1 = match inst {
            Binary(op, arg1, arg2) => Binary(*op, home(arg1), home(arg2)),
            Unary(op, arg) => Unary(*op, home(arg)),
            Retq => Retq,
            Jmp(_) => unimplemented!(),
            JmpIf(..) => unimplemented!(),
            Callq(..) => unimplemented!(),
        };
        list1.push(inst1);
    }
    BlockStack(stack_size, list1)
}

pub fn interp_block_stack(block: &BlockStack) -> Int {
    let BlockStack(stack_size, list) = block;
    let mut frame = vec![];
    frame.resize(*stack_size as usize, 0xDEADBEEF);
    let env = interp_inst(
        &mut frame,
        vec![],
        list.iter().rev().cloned().collect(),
        &BTreeMap::new(),
    );
    *env_get(&env, &EnvKey::Reg(Reg::rax)).unwrap()
}

// ---------------------------------------------------------------------------
// patch instructions

pub fn patch_x86(block: &BlockStack) -> BlockStack {
    let BlockStack(stack_size, list) = block;
    let mut list1 = vec![];
    for inst in list {
        use Inst::*;
        use Reg::{rax, rbp};
        let insts1 = match inst {
            Binary(BinaryKind::Movq, arg1, arg2) if arg1 == arg2 => vec![],
            Binary(op, Arg::Deref(rbp, idx1), Arg::Deref(rbp, idx2)) => vec![
                Binary(BinaryKind::Movq, Arg::Deref(rbp, *idx1), Arg::Reg(rax)),
                Binary(*op, Arg::Reg(rax), Arg::Deref(rbp, *idx2)),
            ],
            _ => vec![inst.clone()],
        };
        for inst in insts1 {
            list1.push(inst)
        }
    }
    BlockStack(*stack_size, list1)
}

// ---------------------------------------------------------------------------
// print x86

fn print_x86arg(arg: &Arg) -> String {
    match arg {
        Arg::Imm(n) => format!("${}", n),
        x @ Arg::Var(_) => panic!("Can't have Arg::Var in final x86 {:?}", x),
        Arg::Reg(reg) => format!("%{:?}", reg),
        Arg::EFlag => String::new(),
        Arg::ByteReg(breg) => format!("%{:?}", breg),
        Arg::Deref(reg, idx) => format!("{}(%{:?})", idx, reg),
    }
}
fn print_x86inst(inst: &Inst) -> String {
    use Inst::*;
    match inst {
        Binary(op, arg1, arg2) => {
            let arg1 = print_x86arg(arg1);
            let arg2 = print_x86arg(arg2);
            let opcode = match op {
                BinaryKind::Addq => "addq",
                BinaryKind::Movq => "movq",
                BinaryKind::Subq => "subq",
                BinaryKind::Cmpq => "cmpq",
                BinaryKind::Xorq => "xorq",
                BinaryKind::Movzbq => "movbzq",
            };
            format!("{}\t{}, {}", opcode, arg1, arg2)
        }
        Unary(op, arg) => {
            let arg = print_x86arg(arg);
            let opcode = match op {
                UnaryKind::Negq => "negq",
                _ => panic!("unhadnled {:?}", inst),
            };
            format!("{}\t{}", opcode, arg)
        }
        _ => panic!("unhandled inst {:?}", inst),
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

#[derive(Debug, Clone)]
pub struct LiveSet(pub Option<String>, pub HashSet<String>);

pub fn liveness_analysis(block: &BlockVar) -> Vec<LiveSet> {
    fn update_with_rdwr(live_set: &mut HashSet<String>, rd: &[&Arg], wr: &Arg) -> Option<String> {
        // before_k = (after_k - wr) + rd;
        match wr {
            Arg::Var(x) => live_set.remove(x),
            _ => false,
        };
        for arg in rd {
            match arg {
                Arg::Var(x) => live_set.insert(x.clone()),
                _ => false,
            };
        }
        match wr {
            Arg::Var(x) => Some(x.clone()),
            _ => None,
        }
    }
    let update_live_set = |inst: &Inst, live_set: &HashSet<String>| {
        let mut live_set = live_set.clone();
        use Inst::*;
        let wr = match inst {
            Binary(op, a1, a2) => match op {
                BinaryKind::Addq => update_with_rdwr(&mut live_set, &[a1, a2], a2),
                BinaryKind::Subq => update_with_rdwr(&mut live_set, &[a1, a2], a2),
                BinaryKind::Movq => update_with_rdwr(&mut live_set, &[a1], a2),
                BinaryKind::Cmpq => unimplemented!(),
                BinaryKind::Xorq => unimplemented!(),
                BinaryKind::Movzbq => unimplemented!(),
            },
            Unary(op, arg) => match op {
                UnaryKind::Negq => update_with_rdwr(&mut live_set, &[arg], arg),
                UnaryKind::Pushq => unimplemented!(),
                UnaryKind::Popq => unimplemented!(),
                UnaryKind::Set(..) => unimplemented!(),
            },
            Callq(_, _) => unimplemented!(),
            Retq => None,
            Jmp(_) => unimplemented!(),
            JmpIf(..) => unimplemented!(),
        };
        LiveSet(wr, live_set)
    };

    let BlockVar(_, list) = block;
    let mut live_set_vec: Vec<LiveSet> = vec![LiveSet(None, HashSet::new())];
    for inst in list.iter().rev() {
        let live_set = update_live_set(inst, &live_set_vec.last().unwrap().1);
        live_set_vec.push(live_set);
    }
    // reverse vector
    live_set_vec.reverse();
    // pop last element, which is empty set
    live_set_vec.pop();
    live_set_vec
}

// ---------------------------------------------------------------------------
// interference graph

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IVertex(pub String);

#[derive(Debug, Clone)]
pub struct IEdge(pub IVertex, pub IVertex);

impl Eq for IEdge {}

// (a,b) = (b,a)
impl PartialEq for IEdge {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1 || self.0 == other.1 && self.1 == other.0
    }
}

// This will make it easier to find the right connections
impl PartialEq<IVertex> for IEdge {
    fn eq(&self, other: &IVertex) -> bool {
        self.0 == *other || self.1 == *other
    }
}

// order such that if (a,b) ~ (b,a)
impl Ord for IEdge {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::{max, min};
        let v0 = (min(&self.0, &self.1), max(&self.0, &self.1));
        let v1 = (min(&other.0, &other.1), max(&other.0, &other.1));
        v0.cmp(&v1)
    }
}

impl PartialOrd for IEdge {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

type IGraph = BTreeSet<IEdge>;
pub fn interference_graph(liveness: &Vec<LiveSet>) -> IGraph {
    let mut g = BTreeSet::new();

    for LiveSet(_wr, set) in liveness {
        // XXX: use o(n^2) algorithm, since the write-based algorithm is incorrectly implemented
        for a in set {
            for b in set {
                g.insert(IEdge(IVertex(a.clone()), IVertex(b.clone())));
            }
        }
        /*
        // write-based algorithm to build interference graph
        if let Some(wr) = wr {
            let mut set = set.clone();
            set.remove(wr);
            for el in set {
                g.insert(IEdge(IVertex(wr.clone()), IVertex(el)));
            }
        }
        */
    }

    g
}

// ---------------------------------------------------------------------------
// graph coloring

pub fn reg_alloc(ig: &IGraph, bg: &IGraph) -> BTreeMap<String, Reg> {
    type Color = usize;
    type WorkSet = BTreeMap<String, BTreeSet<Color>>;
    type ColorMap = BTreeMap<String, Color>;

    let mut workset: WorkSet = BTreeMap::new();
    for s in ig {
        let IEdge(IVertex(a), IVertex(b)) = s.clone();
        workset.insert(a, BTreeSet::new());
        workset.insert(b, BTreeSet::new());
    }

    fn find_candidates(w: &WorkSet) -> BTreeSet<String> {
        let (_, mut satmax) = w.iter().next().unwrap();
        let mut candidates = BTreeSet::<String>::new();
        for (v, sat) in w.iter() {
            if sat.len() == satmax.len() {
                candidates.insert(v.clone());
            } else if sat.len() > satmax.len() {
                satmax = sat;
                candidates = [v.to_string()].iter().cloned().collect();
            }
        }
        candidates
    }

    fn find_adjacent(g: &IGraph, v: &String) -> BTreeSet<String> {
        let mut adjacent = BTreeSet::new();
        for IEdge(IVertex(v1), IVertex(v2)) in g {
            if v1 == v {
                adjacent.insert(v2.clone());
            } else if v2 == v {
                adjacent.insert(v1.clone());
            }
        }
        //println!(" -- v= {:?} adjacent= {:?}", v, adjacent);
        adjacent
    }

    fn color_vertex(
        adjacent: &BTreeSet<String>,
        colormap: &ColorMap,
        color: Option<Color>,
    ) -> Option<Color> {
        let can_use = |color| {
            let mut can_use = true;
            for v in adjacent {
                if let Some(used_col) = colormap.get(v) {
                    can_use = can_use && *used_col != color;
                }
            }
            can_use
        };
        if let Some(color) = color {
            if can_use(color) {
                return Some(color);
            }
        }
        let max_regs = 13;
        for color in 0..max_regs {
            if can_use(color) {
                return Some(color);
            }
        }
        return None;
    }

    fn pick_vertex(
        candidates: &BTreeSet<String>,
        colormap: &ColorMap,
        bg: &IGraph,
    ) -> (String, Option<Color>) {
        for IEdge(IVertex(a), IVertex(b)) in bg {
            /*
            println!(
                "a,b= {:?}, candidates={:?}, colormap={:?} - {:?}",
                (a, b),
                candidates,
                colormap,
                (candidates.get(a), colormap.get(a)),
            );
            */
            if !candidates.get(a).is_none() && !colormap.get(b).is_none() {
                return (a.clone(), Some(*colormap.get(b).unwrap()));
            }
            if !candidates.get(b).is_none() && !colormap.get(a).is_none() {
                return (b.clone(), Some(*colormap.get(a).unwrap()));
            }
        }
        (candidates.iter().next().unwrap().clone(), None)
    }

    let mut colormap: ColorMap = BTreeMap::new();
    while !workset.is_empty() {
        let satset = find_candidates(&mut workset);
        let (v, color) = pick_vertex(&satset, &colormap, bg);
        println!("v= {:?}, candidate_color ={:?}", v, color);
        workset.remove(&v);
        let adjacent = find_adjacent(ig, &v);
        let color = color_vertex(&adjacent, &colormap, color);
        if let Some(color) = color {
            assert_eq!(colormap.insert(v, color), None);
            for v in adjacent {
                if let Some(sat) = workset.get_mut(&v) {
                    sat.insert(color);
                }
            }
        }
    }

    let color2reg = vec![
        Reg::rbx, // 0
        Reg::rcx, // 1
        Reg::rdx, // 2
        Reg::rsi, // 3
        Reg::rdi, // 4
        Reg::r8,  // 5
        Reg::r9,  // 6
        Reg::r10, // 7
        Reg::r11, // 8
        Reg::r12, // 9
        Reg::r13, // 10
        Reg::r14, // 11
        Reg::r15, // 12
    ];

    let mut regs = BTreeMap::new();
    for (v, color) in colormap {
        println!("v= {:?} color={:?}", v, color);
        regs.insert(v, color2reg[color]);
    }
    regs
}

// ---------------------------------------------------------------------------
// move bias graph

pub fn move_bias(b: &BlockVar) -> IGraph {
    let mut g = BTreeSet::new();
    let BlockVar(_, inst_list) = b;
    for inst in inst_list.clone() {
        if let Inst::Binary(BinaryKind::Movq, Arg::Var(x), Arg::Var(y)) = inst {
            g.insert(IEdge(IVertex(x), IVertex(y)));
        }
    }
    g
}
