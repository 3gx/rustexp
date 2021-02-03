#[path = "./macros.rs"]
mod macros;
#[allow(unused_imports)]
use macros::r#match;

#[path = "cvar_lang.rs"]
pub mod cvar_lang;
pub use cvar_lang::rvar_anf_lang;
pub use rvar_anf_lang::rvar_lang;

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
pub struct Options {
    pub vars: Option<Vec<String>>,
    pub regs: Option<BTreeMap<String, Reg>>,
}

#[derive(Debug, Clone)]
pub struct BlockVarOpts {
    pub vars: Vec<String>,
    pub regs: BTreeMap<String, Reg>,
}
#[derive(Debug, Clone)]
pub struct BlockVar(pub BlockVarOpts, pub Vec<Inst>);
#[derive(Debug, Clone)]
pub struct BlockStack(pub Int, pub Vec<Inst>);

impl BlockVar {
    pub fn new() -> BlockVar {
        BlockVar(
            BlockVarOpts {
                vars: vec![],
                regs: BTreeMap::new(),
            },
            vec![],
        )
    }
    pub fn with_vars(mut self, vars: Vec<String>) -> BlockVar {
        self.0.vars = vars;
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

pub fn select_inst_tail(t: &CVarLang::Tail, block: BlockVar) -> BlockVar {
    use CVarLang::Tail;
    use Reg::*;

    match t {
        Tail::Return(expr) => {
            let BlockVar(info, mut list) = block;
            for inst in select_inst_assign(Arg::Reg(rax), expr) {
                list.push(inst)
            }
            //list.push(Retq);
            BlockVar(info, list)
        }
        Tail::Seq(stmt, tail) => {
            let BlockVar(info, mut list) = block;
            for inst in select_inst_stmt(stmt) {
                list.push(inst);
            }
            select_inst_tail(tail, BlockVar(info, list))
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
pub fn interp_block(block: &BlockVar) -> Int {
    let BlockVar(_, list) = block;
    let mut env: Env = vec![];
    let mut frame = vec![];
    for inst in list {
        env = interp_inst(&mut frame, env, inst);
    }
    *env_get(&env, &EnvKey::Reg(Reg::rax)).unwrap()
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

// ---------------------------------------------------------------------------
// patch instructions

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

// ---------------------------------------------------------------------------
// print x86

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
            Addq(a1, a2) => update_with_rdwr(&mut live_set, &[a1, a2], a2),
            Subq(a1, a2) => update_with_rdwr(&mut live_set, &[a1, a2], a2),
            Movq(a1, a2) => update_with_rdwr(&mut live_set, &[a1], a2),
            Negq(arg) => update_with_rdwr(&mut live_set, &[arg], arg),
            Callq(_, _) => unimplemented!(),
            Retq => None,
            Pushq(_) => unimplemented!(),
            Popq(_) => unimplemented!(),
            Jmp(_) => unimplemented!(),
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

    for LiveSet(wr, set) in liveness {
        if let Some(wr) = wr {
            let mut set = set.clone();
            set.remove(wr);
            for el in set {
                g.insert(IEdge(IVertex(wr.clone()), IVertex(el)));
            }
        }
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
        println!(" -- v= {:?} adjacent= {:?}", v, adjacent);
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
        let max_regs = 12;
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
        if let Inst::Movq(Arg::Var(x), Arg::Var(y)) = inst {
            g.insert(IEdge(IVertex(x), IVertex(y)));
        }
    }
    g
}
