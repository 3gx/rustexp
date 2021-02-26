#[path = "./macros.rs"]
mod macros;
#[allow(unused_imports)]
use macros::r#match;

pub use petgraph::stable_graph::StableGraph;

#[path = "cvar_lang.rs"]
pub mod cvar_lang;
pub use cvar_lang as CVar;
pub use CVar::rvar_anf_lang as RVarAnf;
pub use RVarAnf::rvar_lang as RVarLang;

type Int = i64;
type Bool = bool;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Value {
    Int(Int),
    EFlag(CndCode),
}

impl From<RVarAnf::Value> for Value {
    fn from(item: RVarAnf::Value) -> Self {
        match item {
            RVarAnf::Value::Int(i) => Value::Int(i),
            _ => panic!("cannot cast from bool: {:?}", item),
        }
    }
}

impl Value {
    pub fn int(&self) -> Option<&Int> {
        match self {
            Value::Int(n) => Some(n),
            Value::EFlag(_) => None,
        }
    }
}

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
    EFlag,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CndCode {
    Eq,
    Lt,
    Gt,
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
pub struct BasicBlock {
    name: String,
    vars: BTreeSet<String>,
    liveset: Vec<LiveSet>,
    insts: Vec<Inst>,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        BasicBlock {
            name,
            vars: BTreeSet::new(),
            liveset: vec![],
            insts: vec![],
        }
    }
    pub fn vars(mut self, vars: BTreeSet<String>) -> Self {
        self.vars = vars;
        self
    }
    pub fn liveset(mut self, liveset: Vec<LiveSet>) -> Self {
        self.liveset = liveset;
        self
    }
    pub fn insts(mut self, insts: Vec<Inst>) -> Self {
        self.insts = insts;
        self
    }
}

impl std::default::Default for BasicBlock {
    fn default() -> Self {
        BasicBlock::new("".to_string())
    }
}

#[derive(Debug, Clone)]
pub struct Options {
    pub stack: Int,
    pub vars: BTreeSet<String>,
    pub regs: BTreeMap<String, Reg>,
}
pub type CfgGraph = StableGraph<BasicBlock, HashSet<String>>;
#[derive(Debug, Clone)]
pub struct Cfg(pub Options, pub CfgGraph);

impl Cfg {
    pub fn regs(mut self, regs: BTreeMap<String, Reg>) -> Self {
        self.0.regs = regs;
        self
    }
}

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
                let cond = match cmpop {
                    CVar::BinaryOpKind::Eq => CndCode::Eq,
                    CVar::BinaryOpKind::Lt => CndCode::Lt,
                    x @ _ => panic!("unhandled 'if' binary predicate {:?}", x),
                };
                let insts = vec![
                    Inst::Binary(BinaryKind::Cmpq, a2, a1),
                    Inst::JmpIf(cond, thn.clone()),
                    Inst::Jmp(els.clone()),
                ];
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
                let insts = vec![
                    Inst::Binary(BinaryKind::Xorq, a2.clone(), a1.clone()),
                    Inst::Binary(BinaryKind::Cmpq, a3, a1),
                    Inst::JmpIf(CndCode::Eq, els.clone()),
                    Inst::Jmp(thn.clone()),
                ];

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
                let insts = vec![
                    Inst::Binary(BinaryKind::Cmpq, a2, a1),
                    Inst::JmpIf(CndCode::Eq, els.clone()),
                    Inst::Jmp(thn.clone()),
                ];
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

pub fn select_inst_prog(cprog: CVar::CProgram) -> Cfg {
    let CVar::CProgram(bbs) = cprog;
    let mut x86bbs = Vec::new();
    let mut all_vars = BTreeSet::new();
    for CVar::BasicBlock(name, tail) in bbs {
        let BlockVar(BlockVarOpts { vars, regs }, insts) = select_inst_tail(&tail, BlockVar::new());
        assert_eq!(regs.len(), 0);
        for var in &vars {
            all_vars.insert(var.clone());
        }
        x86bbs.push(BasicBlock::new(name).vars(vars).insts(insts))
    }
    let opts = Options {
        stack: 0,
        vars: all_vars,
        regs: BTreeMap::new(),
    };
    let bbs = x86bbs;
    let mut cfg = CfgGraph::new();
    let name2node: HashMap<_, _> = bbs
        .into_iter()
        .map(|bb| {
            let name = bb.name.clone();
            let idx = cfg.add_node(bb);
            (name, idx)
        })
        .collect();
    for (_, src_idx) in &name2node {
        let insts = &cfg[*src_idx].insts;
        let nodes: BTreeSet<_> = insts
            .iter()
            .filter_map(|inst| match inst {
                Inst::Jmp(label) => Some(label),
                Inst::JmpIf(_, label) => Some(label),
                Inst::Unary(..) => None,
                Inst::Binary(..) => None,
                Inst::Callq(..) => None,
                Inst::Retq => None,
            })
            .map(|label| name2node.get(label))
            .collect();
        for dst_idx in nodes {
            let dst_idx = dst_idx.unwrap();
            assert!(cfg.find_edge(*src_idx, *dst_idx).is_none());
            cfg.add_edge(*src_idx, *dst_idx, HashSet::new());
        }
    }

    Cfg(opts, cfg)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnvKey {
    Reg(Reg),
    ByteReg(ByteReg),
    Var(String),
    EFlag,
}

pub type Env = Vec<(EnvKey, Value)>;
fn env_get<'a>(env: &'a Env, reg: &EnvKey) -> Option<&'a Value> {
    env.iter()
        .rev()
        .find_map(|x| if &x.0 != reg { None } else { Some(&x.1) })
}

fn env_set(mut env: Env, reg: EnvKey, val: Value) -> Env {
    env.push((reg, val));
    env
}

fn interp_arg(frame: &Vec<Value>, env: &Env, arg: &Arg) -> Value {
    match arg {
        Arg::Imm(n) => Value::Int(*n),
        Arg::Reg(reg) => *env_get(env, &EnvKey::Reg(*reg)).unwrap(),
        Arg::ByteReg(breg) => *env_get(env, &EnvKey::ByteReg(*breg)).unwrap(),
        Arg::Var(x) => *env_get(env, &EnvKey::Var(x.clone())).unwrap(),
        Arg::EFlag => *env_get(env, &EnvKey::EFlag).unwrap(),
        Arg::Deref(Reg::rbp, idx) => frame[(-idx - 8) as usize],
        Arg::Deref(..) => panic!("unimplemented {:?}", arg),
    }
}

pub fn interp_inst(
    frame: &mut Vec<Value>,
    env: Env,
    mut insts: Vec<Inst>,
    prog: &BTreeMap<String, Vec<Inst>>,
) -> Env {
    use Inst::*;

    fn assign(frame: &mut Vec<Value>, env: Env, arg: &Arg, result: Value) -> Option<Env> {
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
                let result = interp_arg(frame, &env, arg1).int().unwrap()
                    + interp_arg(frame, &env, arg2).int().unwrap();
                let env = assign(frame, env, arg2, Value::Int(result)).unwrap();
                interp_inst(frame, env, insts, prog)
            }
            BinaryKind::Xorq => {
                let result = interp_arg(frame, &env, arg1).int().unwrap()
                    ^ interp_arg(frame, &env, arg2).int().unwrap();
                let env = assign(frame, env, arg2, Value::Int(result)).unwrap();
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
                let arg1 = *interp_arg(frame, &env, arg1).int().unwrap();
                let arg2 = *interp_arg(frame, &env, arg2).int().unwrap();
                let eflag = if arg1 == arg2 {
                    CndCode::Eq
                } else if arg1 < arg2 {
                    CndCode::Lt
                } else {
                    CndCode::Gt
                };
                let env = assign(frame, env, &Arg::EFlag, Value::EFlag(eflag)).unwrap();
                interp_inst(frame, env, insts, prog)
            }
            BinaryKind::Subq => panic!("unsupported instruction{:?}", inst),
        },
        Unary(op, arg) => match op {
            UnaryKind::Negq => {
                let result = -interp_arg(frame, &env, arg).int().unwrap();
                let env = assign(frame, env, arg, Value::Int(result)).unwrap();
                interp_inst(frame, env, insts, prog)
            }
            UnaryKind::Pushq => panic!("unsupported instruction{:?}", inst),
            UnaryKind::Popq => panic!("unsupported instruction{:?}", inst),
            UnaryKind::Set(cc) => {
                let eflag = interp_arg(frame, &env, &Arg::EFlag);
                let eflag = Value::EFlag(*cc) == eflag;
                let env = assign(frame, env, arg, Value::Int(eflag as Int)).unwrap();
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
            if Value::EFlag(*cc) == eflag {
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

pub fn interp_cfg(prog: &Cfg) -> Value {
    let Cfg(
        Options {
            stack,
            vars: _,
            regs: _,
        },
        cfg,
    ) = prog;
    let mut prog = BTreeMap::new();
    for bb in cfg.node_indices().map(|idx| &cfg[idx]).cloned() {
        prog.insert(bb.name, bb.insts);
    }
    let insts = prog.get(&"start".to_string()).unwrap();
    let mut env: Env = vec![];
    let mut frame = vec![];
    frame.resize(*stack as usize, Value::Int(0));
    env = interp_inst(
        &mut frame,
        env,
        insts.iter().rev().cloned().collect(),
        &prog,
    );
    *env_get(&env, &EnvKey::Reg(Reg::rax)).unwrap()
}

pub fn assign_homes_cfg(prog: Cfg) -> Cfg {
    let Cfg(
        Options {
            stack: _,
            vars,
            regs,
        },
        cfg,
    ) = prog;
    let mut var2idx: HashMap<String, Int> = HashMap::new();
    let mut stack = 0;
    for var in &vars {
        assert!(!var2idx.contains_key(var));
        if regs.get(var).is_none() {
            stack += 8;
            var2idx.insert(var.clone(), stack);
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
    let cfg = {
        let mut cfg = cfg;
        for bb in cfg.node_weights_mut() {
            let mut new_insts = vec![];
            for inst in bb.insts.iter_mut() {
                use Inst::*;
                let new_inst = match inst {
                    Binary(op, arg1, arg2) => Binary(*op, home(&arg1), home(&arg2)),
                    Unary(op, arg) => Unary(*op, home(&arg)),
                    x @ _ => x.clone(),
                };
                new_insts.push(new_inst);
            }
            bb.insts = new_insts;
        }
        cfg
    };
    Cfg(Options { stack, vars, regs }, cfg)
}

// ---------------------------------------------------------------------------
// patch instructions

pub fn patch_cfg(prog: Cfg) -> Cfg {
    let Cfg(Options { stack, vars, regs }, mut cfg) = prog;
    cfg.node_weights_mut().for_each(|bb| {
        bb.insts = std::mem::take(&mut bb.insts)
            .into_iter()
            .map(|inst| {
                use Inst::*;
                use Reg::{rax, rbp};
                match inst {
                    Binary(BinaryKind::Movq, arg1, arg2) if arg1 == arg2 => vec![],
                    Binary(BinaryKind::Movzbq, arg, Arg::Deref(rbp, idx)) => vec![
                        Binary(BinaryKind::Movzbq, arg, Arg::Reg(rax)),
                        Binary(BinaryKind::Movq, Arg::Reg(rax), Arg::Deref(rbp, idx)),
                    ],
                    Binary(op, Arg::Deref(rbp, idx1), Arg::Deref(rbp, idx2)) => vec![
                        Binary(BinaryKind::Movq, Arg::Deref(rbp, idx1), Arg::Reg(rax)),
                        Binary(op, Arg::Reg(rax), Arg::Deref(rbp, idx2)),
                    ],
                    x @ _ => vec![x],
                }
            })
            .flatten()
            .collect()
    });
    Cfg(Options { stack, vars, regs }, cfg)
}

// ---------------------------------------------------------------------------
// print x86

fn print_x86arg(arg: &Arg) -> String {
    match arg {
        Arg::Imm(n) => format!("${}", n),
        Arg::Var(x) => format!("{}", x),
        Arg::Reg(reg) => format!("%{:?}", reg),
        Arg::EFlag => String::new(),
        Arg::ByteReg(breg) => format!("%{:?}", breg),
        Arg::Deref(reg, idx) => format!("{}(%{:?})", idx, reg),
    }
}
fn print_x86cc(cc: &CndCode) -> &str {
    match cc {
        CndCode::Eq => "e",
        CndCode::Lt => "l",
        CndCode::Gt => "g",
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
                BinaryKind::Movzbq => "movzbq",
            };
            format!("{}\t{}, {}", opcode, arg1, arg2)
        }
        Unary(op, arg) => {
            let arg = print_x86arg(arg);
            let opcode = match op {
                UnaryKind::Negq => "negq".to_string(),
                UnaryKind::Set(cc) => format!("set{}", print_x86cc(cc)),
                _ => panic!("unhadnled {:?}", inst),
            };
            format!("{}\t{}", opcode, arg)
        }
        Callq(_label, _) => {
            //format!("callq\t{}", label),
            format!("int\t$3")
        }
        Retq => unimplemented!(),
        Jmp(label) => format!("jmp\t{}", label),
        JmpIf(cc, label) => format!("j{}\t{}", print_x86cc(cc), label),
    }
}

pub fn print_x86prog(prog: &Cfg) -> String {
    let Cfg(
        Options {
            stack,
            vars: _,
            regs: _,
        },
        cfg,
    ) = prog;

    let mut prog: String = cfg
        .node_indices()
        .map(|idx| {
            (
                &cfg[idx],
                cfg.neighbors_directed(idx, petgraph::Outgoing).next(),
            )
        })
        .map(|(bb, ngb)| {
            let mut prog = String::new();
            // label
            prog.push_str(format!("{}:\n", bb.name).as_str());

            // instruction sequences
            let insts_str = bb
                .insts
                .iter()
                .map(|inst| {
                    let inst_str = print_x86inst(inst);
                    "\t".to_string().to_string() + &inst_str + "\n"
                })
                .collect::<String>();
            prog.push_str(&insts_str);

            // if these are roots of Cfg , jump to conclusion at the end
            if ngb.is_none() {
                prog.push_str("\tjmp\tconclusion\n");
            }

            // prettify
            prog.push_str("\n");
            prog
        })
        .collect();
    prog.push_str("\n");
    prog.push_str("\t.globl main\n");
    prog.push_str("main:\n");
    prog.push_str("\tpush %rbp\n");
    prog.push_str("\tmovq\t%rsp,%rbp\n");
    prog.push_str(format!("\tsubq\t${},%rsp\n", stack).as_str());
    prog.push_str("\tjmp start\n");
    prog.push_str("\n");
    prog.push_str("conclusion:\n");
    prog.push_str(format!("\taddq\t ${}, %rsp\n", stack).as_str());
    prog.push_str("\tpopq\t%rbp\n");
    prog.push_str("\tretq\n");
    prog
}

// ---------------------------------------------------------------------------
// liveness analysis

#[derive(Debug, Clone)]
pub struct LiveSet(pub Option<String>, pub HashSet<String>);

impl LiveSet {
    pub fn new() -> Self {
        LiveSet(None, HashSet::new())
    }
    pub fn liveset(mut self, liveset: HashSet<String>) -> Self {
        self.1 = liveset;
        self
    }
}

fn liveness_analysis_bb(block: &BasicBlock, liveset: LiveSet) -> Vec<LiveSet> {
    fn update_with_rdwr(
        live_set: &mut HashSet<String>,
        rd: &[&Arg],
        wr: Option<&Arg>,
    ) -> Option<String> {
        // before_k = (after_k - wr) + rd;
        match wr {
            Some(Arg::Var(x)) => live_set.remove(x),
            _ => false,
        };
        for arg in rd {
            match arg {
                Arg::Var(x) => live_set.insert(x.clone()),
                _ => false,
            };
        }
        match wr {
            Some(Arg::Var(x)) => Some(x.clone()),
            _ => None,
        }
    }
    let update_live_set = |inst: &Inst, live_set: &HashSet<String>| {
        let mut live_set = live_set.clone();
        use Inst::*;
        let wr = match inst {
            Binary(op, a1, a2) => match op {
                BinaryKind::Addq => update_with_rdwr(&mut live_set, &[a1, a2], Some(a2)),
                BinaryKind::Subq => update_with_rdwr(&mut live_set, &[a1, a2], Some(a2)),
                BinaryKind::Movq => update_with_rdwr(&mut live_set, &[a1], Some(a2)),
                BinaryKind::Cmpq => update_with_rdwr(&mut live_set, &[a1, a2], None),
                BinaryKind::Xorq => update_with_rdwr(&mut live_set, &[a1], Some(a2)),
                BinaryKind::Movzbq => update_with_rdwr(&mut live_set, &[a1], Some(a2)),
            },
            Unary(op, arg) => match op {
                UnaryKind::Negq => update_with_rdwr(&mut live_set, &[arg], Some(arg)),
                UnaryKind::Pushq => unimplemented!(),
                UnaryKind::Popq => unimplemented!(),
                UnaryKind::Set(_) => update_with_rdwr(&mut live_set, &[], Some(arg)),
            },
            Callq(..) => None,
            Retq => None,
            Jmp(..) => None,
            JmpIf(..) => None,
        };
        LiveSet(wr, live_set)
    };

    let mut live_set_vec: Vec<LiveSet> = vec![liveset];
    for inst in block.insts.iter().rev() {
        let live_set = update_live_set(inst, &live_set_vec.last().unwrap().1);
        live_set_vec.push(live_set);
    }
    // reverse vector
    live_set_vec.reverse();
    // pop last element, which is empty set
    live_set_vec.pop();
    live_set_vec
}

pub fn liveness_analysis_cfg(prog: Cfg) -> Cfg {
    let Cfg(Options { stack, vars, regs }, cfg) = prog;

    let (mut cfg, rto_indices) = {
        let mut cfg = petgraph::Graph::from(cfg);
        cfg.reverse();
        let rto = petgraph::algo::toposort(&cfg, None).unwrap();
        cfg.reverse();
        (CfgGraph::from(cfg), rto)
    };

    let get_edges_idx = |cfg: &CfgGraph, node_idx, dir| {
        use petgraph::visit::EdgeRef;
        let edges_idx = cfg
            .edges_directed(node_idx, dir)
            .map(|edge| edge.id())
            .collect::<Vec<_>>();
        edges_idx
    };

    rto_indices.into_iter().for_each(|node_idx| {
        // get current bb from the Cfg

        // build live set from successor BBs
        let liveset = get_edges_idx(&cfg, node_idx, petgraph::Direction::Outgoing)
            .into_iter()
            .map(|idx| {
                cfg.edge_weight(idx)
                    .unwrap()
                    .iter()
                    .cloned()
                    .collect::<Vec<String>>()
            })
            .flatten()
            .collect::<HashSet<String>>();

        let bb = &cfg[node_idx];
        // run liveness analysis of current bb
        let lives = liveness_analysis_bb(
            &BasicBlock::new("".to_string()).insts(bb.insts.clone()),
            LiveSet::new().liveset(liveset),
        );

        // assign live-set at entry to BB to incoming Cfg edges
        let LiveSet(_, liveset_at_entry) = &lives[0];
        get_edges_idx(&cfg, node_idx, petgraph::Direction::Incoming)
            .into_iter()
            .for_each(|idx| {
                let edge = cfg.edge_weight_mut(idx).unwrap();
                *edge = liveset_at_entry.clone();
            });

        // update bbopts
        let bb = std::mem::take(&mut cfg[node_idx]);
        std::mem::swap(&mut cfg[node_idx], &mut bb.liveset(lives));
    });
    Cfg(Options { stack, vars, regs }, cfg)
}

// ---------------------------------------------------------------------------
// interference graph

#[derive(Debug, Clone)]
pub struct IGraph(
    pub HashMap<String, petgraph::prelude::NodeIndex>,
    pub StableGraph<String, (), petgraph::Undirected>,
);

impl IGraph {
    pub fn node_index(&self, n: &str) -> Option<petgraph::prelude::NodeIndex> {
        self.0.get(n).map(|x| *x)
    }
}

pub fn interference_graph_cfg(cfg: &Cfg) -> IGraph {
    let Cfg(_, cfg) = cfg;
    let mut g = StableGraph::default();
    let mut var2node = HashMap::new();
    for node_idx in cfg.node_indices() {
        let bb = &cfg[node_idx];
        let liveness = &bb.liveset;
        for LiveSet(_, set) in liveness {
            for a in set {
                for b in set {
                    let v1 = *var2node
                        .entry(a.clone())
                        .or_insert_with(|| g.add_node(a.clone()));
                    let v2 = *var2node
                        .entry(b.clone())
                        .or_insert_with(|| g.add_node(b.clone()));
                    g.add_edge(v1, v2, ());
                }
            }
        }
    }
    IGraph(var2node, g)
}

// ---------------------------------------------------------------------------
// move bias graph

pub fn move_bias_cfg(cfg: &Cfg) -> IGraph {
    let Cfg(_, cfg) = cfg;
    let mut g = StableGraph::default();
    let mut var2node = HashMap::new();
    for node_idx in cfg.node_indices() {
        let bb = &cfg[node_idx];
        for inst in &bb.insts {
            if let Inst::Binary(BinaryKind::Movq, Arg::Var(x), Arg::Var(y)) = inst {
                let v1 = *var2node
                    .entry(x.clone())
                    .or_insert_with(|| g.add_node(x.clone()));
                let v2 = *var2node
                    .entry(y.clone())
                    .or_insert_with(|| g.add_node(y.clone()));
                g.add_edge(v1, v2, ());
            }
        }
    }
    IGraph(var2node, g)
}

// ---------------------------------------------------------------------------
// graph coloring

pub fn reg_alloc_g(ig: &IGraph, bg: &IGraph) -> BTreeMap<String, Reg> {
    type Color = usize;
    type WorkSet = BTreeMap<String, BTreeSet<Color>>;
    type ColorMap = BTreeMap<String, Color>;

    let mut workset: WorkSet = BTreeMap::new();
    for idx in ig.1.node_indices() {
        workset.insert(ig.1[idx].clone(), BTreeSet::new());
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
        if let Some(idx) = g.node_index(v) {
            for ngb in g.1.neighbors(idx) {
                adjacent.insert(g.1[ngb].clone());
            }
        }
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
        for edge_idx in bg.1.edge_indices() {
            let (src, tgt) = bg.1.edge_endpoints(edge_idx).unwrap();
            let a = &bg.1[src];
            let b = &bg.1[tgt];
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
