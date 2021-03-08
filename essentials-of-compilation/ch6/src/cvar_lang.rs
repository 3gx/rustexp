#[path = "rvar_anf_lang.rs"]
pub mod rvar_anf_lang;
pub use rvar_anf_lang as RVarAnf;
pub use RVarAnf::rvar_lang as RVar;

#[path = "./macros.rs"]
mod macros;
use macros::r#match;

use RVar::{gensym, /*gensym_reset,*/ sym_get, sym_set, Env};
use RVarAnf::{interp_atom, Value};
pub use RVarAnf::{type_size_in_bytes, BinaryOpKind, Bool, Int, Type, UnaryOpKind};

#[derive(Debug, Clone)]
pub enum Atom {
    Int(Int),
    Bool(Bool),
    Var(String),
    Void,
}

pub macro int($e:expr) {
    Atom::Int($e)
}
pub macro var {
    ($id:ident) => { Atom::Var(stringify!($id).to_string()) },
    ($e:expr) => { $e.to_string().as_str().into_term() },
}

impl From<Atom> for RVarAnf::Atom {
    fn from(item: Atom) -> Self {
        match item {
            Atom::Int(i) => RVarAnf::Atom::Int(i),
            Atom::Bool(b) => RVarAnf::Atom::Bool(b),
            Atom::Var(v) => RVarAnf::Atom::Var(v),
            Atom::Void => RVarAnf::Atom::Void,
        }
    }
}

impl From<RVarAnf::Atom> for Atom {
    fn from(item: RVarAnf::Atom) -> Self {
        match item {
            RVarAnf::Atom::Int(i) => Atom::Int(i),
            RVarAnf::Atom::Bool(b) => Atom::Bool(b),
            RVarAnf::Atom::Var(v) => Atom::Var(v),
            RVarAnf::Atom::Void => Atom::Void,
        }
    }
}
impl From<&Atom> for RVarAnf::Atom {
    fn from(item: &Atom) -> Self {
        item.clone().into()
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(Atom),
    Read,
    UnaryOp(UnaryOpKind, Atom),
    BinaryOp(BinaryOpKind, Atom, Atom),
    Allocate(Int, Type),
    TupleRef(Atom, Int),
    TupleSet(Atom, Int, Atom),
    GlobalVar(String),
    Void,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    AssignVar(String, Expr),
    Collect(Int),
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
pub struct BasicBlock {
    pub name: String,
    pub tail: Tail,
}

impl BasicBlock {
    fn new(name: &str, tail: Tail) -> Self {
        BasicBlock {
            name: name.to_string(),
            tail,
        }
    }
}

pub use petgraph::stable_graph::StableGraph;
pub use std::collections::HashMap;
pub type CfgGraph = StableGraph<BasicBlock, ()>;
#[derive(Debug, Clone)]
pub struct Program {
    pub global_vars: BTreeMap<String, Value>,
    pub local_vars: BTreeMap<String, Type>,
    pub bb2node: HashMap<String, petgraph::prelude::NodeIndex>,
    pub cfg: CfgGraph,
}
impl Program {
    fn new() -> Self {
        Program {
            global_vars: BTreeMap::default(),
            local_vars: BTreeMap::default(),
            bb2node: HashMap::default(),
            cfg: CfgGraph::default(),
        }
    }
    fn get_bb<'a>(&'a self, name: &str) -> Option<&'a BasicBlock> {
        let node_idx = self.bb2node.get(name)?;
        self.cfg.node_weight(*node_idx)
    }
    fn add_bb(mut self, bb: BasicBlock) -> Self {
        let name = bb.name.clone();
        let node_idx = self.cfg.add_node(bb);
        assert!(self.bb2node.insert(name, node_idx).is_none());
        self
    }
    fn add_local_var(mut self, var: &str, ty: Type) -> Self {
        match self.local_vars.get(var) {
            Some(vty) => assert_eq!(vty, &ty),
            _ => (),
        };
        self.local_vars.insert(var.to_string(), ty);
        self
    }
}

pub fn interp_expr(env: &Env, e: &Expr) -> Value {
    use Expr::*;
    match e {
        Atom(atom) => interp_atom(env, &atom.into()),
        Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            Value::Int(input.trim().parse().unwrap())
        }
        UnaryOp(op, a) => match (op, interp_atom(env, &a.into())) {
            (UnaryOpKind::Not, Value::Bool(b)) => Value::Bool(!b),
            (UnaryOpKind::Neg, Value::Int(i)) => Value::Int(-i),
            x @ _ => panic!("type mismatch: {:?}", x),
        },
        BinaryOp(op, a1, a2) => {
            match (
                op,
                interp_atom(env, &a1.into()),
                interp_atom(env, &a2.into()),
            ) {
                (BinaryOpKind::Add, Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                (BinaryOpKind::Eq, Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
                (BinaryOpKind::Eq, Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
                (BinaryOpKind::Lt, Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                x @ _ => panic!("type mismatch: {:?}", x),
            }
        }
        Allocate(1, Type::Tuple(ty)) => {
            let mut val = Vec::new();
            val.resize(ty.len(), Value::default());
            Value::Tuple(val).onheap()
        }
        x @ Allocate(..) => panic!("unimplemented {:?}", x),
        TupleRef(tu, idx) => {
            r#match! { [interp_atom(env, &tu.into())]
                Value::Heap(el) if @{let Value::Tuple(tu) = &mut *el.borrow_mut()} =>
                    tu[*idx as usize].clone(),
                _ => panic!("expecting tuple, but got  {:?}", tu)
            }
        }
        TupleSet(tu, idx, val) => {
            r#match! { [(interp_atom(env, &tu.into()), interp_atom(env, &val.into()))]
                (Value::Heap(el), val) if @{let Value::Tuple(tu) = &mut *el.borrow_mut()} =>
                    tu[*idx as usize] = val,
                _ => panic!("expecting tuple, but got  {:?}", tu)
            };
            Value::Void
        }
        GlobalVar(var) => {
            r#match! { [sym_get(env, var)]
                Some(x) if @{let Value::Heap(x) = x} => x.borrow().clone(),
                _ => panic!("unknown globalvar {:?}", var)
            }
        }
        Void => unimplemented!(),
    }
}

pub fn interp_stmt(env: &Env, stmt: &Stmt) -> Env {
    match stmt {
        Stmt::AssignVar(var, exp) => sym_set(env, var, &interp_expr(env, exp)),
        Stmt::Collect(bytes) => {
            r#match! { [sym_get(env, "fromspace_end")]
                Some(x) if @{let Value::Heap(x) = x,
                             let Value::Int(end) = &mut *x.borrow_mut()} =>
                         {*end = ((bytes as &Int) + *end)*2;},
                x@_ => panic!("fromspace_end is not valid, got {:?}", x)
            }
            println!("\ncall to collect");
            env.clone()
        }
    }
}

pub fn interp_tail(env: &Env, tail: &Tail, prog: &Program) -> Value {
    match tail {
        Tail::Return(exp) => interp_expr(env, exp),
        Tail::Seq(stmt, tail) => {
            let new_env = interp_stmt(env, stmt);
            interp_tail(&new_env, tail, prog)
        }
        Tail::Goto(label) => interp_tail(env, &prog.get_bb(label).unwrap().tail, prog),
        Tail::IfStmt(pred, thn, els) => match interp_expr(env, pred) {
            Value::Bool(true) => interp_tail(env, &prog.get_bb(thn).unwrap().tail, prog),
            Value::Bool(false) => interp_tail(env, &prog.get_bb(els).unwrap().tail, prog),
            x @ _ => panic!("predicate must be Bool, got {:?}", x),
        },
    }
}

use std::collections::BTreeMap;
pub fn interp_prog(prog: &Program) -> Value {
    let env = [
        ("free_ptr".to_string(), Value::Int(0).onheap()),
        ("fromspace_end".to_string(), Value::Int(0).onheap()),
    ]
    .iter()
    .cloned()
    .collect();
    let start = prog.get_bb("start").unwrap();
    interp_tail(&env, &start.tail, &prog)
}

fn explicate_ifpred(
    RVarAnf::Expr(e, _ty): RVarAnf::Expr,
    then_name: &str,
    else_name: &str,
    prog: Program,
) -> (BasicBlock, Program) {
    let ret = |t, prog| (BasicBlock::new("", t), prog);
    match e {
        RVarAnf::ExprK::UnaryOp(UnaryOpKind::Not, a) => ret(
            Tail::IfStmt(
                Expr::UnaryOp(UnaryOpKind::Not, a.into()),
                then_name.to_string(),
                else_name.to_string(),
            ),
            prog,
        ),
        RVarAnf::ExprK::BinaryOp(cmp, a1, a2)
            if cmp == BinaryOpKind::Eq || cmp == BinaryOpKind::Lt =>
        {
            ret(
                Tail::IfStmt(
                    Expr::BinaryOp(cmp, a1.into(), a2.into()),
                    then_name.to_string(),
                    else_name.to_string(),
                ),
                prog,
            )
        }
        RVarAnf::ExprK::Atom(RVarAnf::Atom::Bool(pred)) => ret(
            Tail::Goto(if pred {
                then_name.to_string()
            } else {
                else_name.to_string()
            }),
            prog,
        ),
        RVarAnf::ExprK::Atom(a @ RVarAnf::Atom::Var(_)) => ret(
            Tail::IfStmt(
                Expr::Atom(a.into()),
                then_name.to_string(),
                else_name.to_string(),
            ),
            prog,
        ),
        RVarAnf::ExprK::If(p_expr, t_expr, e_expr) => {
            let (then_bb, prog) = explicate_ifpred(*t_expr, then_name, else_name, prog);
            let (else_bb, prog) = explicate_ifpred(*e_expr, then_name, else_name, prog);
            let then_name = gensym("then_bb");
            let else_name = gensym("else_bb");
            let prog = prog.add_bb(BasicBlock::new(&then_name, then_bb.tail));
            let prog = prog.add_bb(BasicBlock::new(&else_name, else_bb.tail));
            explicate_ifpred(*p_expr, &then_name, &else_name, prog)
        }
        RVarAnf::ExprK::Let(x, expr, body) => {
            let (if_tail, prog) = explicate_ifpred(*body, then_name, else_name, prog);
            explicate_assign(&x, *expr, if_tail, prog)
        }
        RVarAnf::ExprK::TupleRef(a, i) => ret(
            Tail::IfStmt(
                Expr::TupleRef(a.into(), i),
                then_name.to_string(),
                else_name.to_string(),
            ),
            prog,
        ),
        x @ _ => panic!("invalid 'if' predicate= {:?}", x),
    }
}

fn explicate_tail(RVarAnf::Expr(e, _ty): RVarAnf::Expr, prog: Program) -> (BasicBlock, Program) {
    let ret = |e, prog| (BasicBlock::new("", Tail::Return(e)), prog);
    match e {
        RVarAnf::ExprK::Atom(a) => ret(Expr::Atom(a.into()), prog),
        RVarAnf::ExprK::Read => ret(Expr::Read, prog),
        RVarAnf::ExprK::UnaryOp(op, a) => ret(Expr::UnaryOp(op, a.into()), prog),
        RVarAnf::ExprK::BinaryOp(op, a1, a2) => ret(Expr::BinaryOp(op, a1.into(), a2.into()), prog),
        RVarAnf::ExprK::Let(x, expr, body) => {
            let (bb, prog) = explicate_tail(*body, prog);
            explicate_assign(&x, *expr, bb, prog)
        }
        RVarAnf::ExprK::If(cnd, thn, els) => {
            let (then_bb, prog) = explicate_tail(*thn, prog);
            let (else_bb, prog) = explicate_tail(*els, prog);
            let then_name = &gensym("then_bb");
            let else_name = &gensym("else_bb");
            let prog = prog.add_bb(BasicBlock::new(&then_name, then_bb.tail));
            let prog = prog.add_bb(BasicBlock::new(&else_name, else_bb.tail));
            explicate_ifpred(*cnd, then_name, else_name, prog)
        }
        RVarAnf::ExprK::Allocate(..) => unimplemented!(),
        RVarAnf::ExprK::Collect(..) => unimplemented!(),
        RVarAnf::ExprK::GlobalVar(..) => unimplemented!(),
        RVarAnf::ExprK::TupleRef(a, i) => ret(Expr::TupleRef(a.into(), i), prog),
        RVarAnf::ExprK::TupleSet(..) => unimplemented!(),
    }
}

fn explicate_assign(
    var: &str,
    RVarAnf::Expr(e, ty): RVarAnf::Expr,
    bb: BasicBlock,
    prog: Program,
) -> (BasicBlock, Program) {
    let assign = |e, bb: BasicBlock, prog: Program| {
        (
            BasicBlock::new(
                "",
                Tail::Seq(Stmt::AssignVar(var.to_string(), e), Box::new(bb.tail)),
            ),
            prog.add_local_var(var, ty),
        )
    };
    match e {
        RVarAnf::ExprK::Atom(a) => assign(Expr::Atom(a.into()), bb, prog),
        RVarAnf::ExprK::Read => assign(Expr::Read, bb, prog),
        RVarAnf::ExprK::UnaryOp(op, a) => assign(Expr::UnaryOp(op, a.into()), bb, prog),
        RVarAnf::ExprK::BinaryOp(op, a1, a2) => {
            assign(Expr::BinaryOp(op, a1.into(), a2.into()), bb, prog)
        }
        RVarAnf::ExprK::Let(x, expr, body) => {
            let (bb, prog) = explicate_assign(var, *body, bb, prog);
            explicate_assign(&x, *expr, bb, prog)
        }
        RVarAnf::ExprK::If(cnd, thn, els) => {
            let tail_name = &gensym("tail_bb");
            let prog = prog.add_bb(BasicBlock::new(&tail_name, bb.tail));
            let (then_bb, prog) = explicate_assign(
                var,
                *thn,
                BasicBlock::new("", Tail::Goto(tail_name.clone())),
                prog,
            );
            let (else_bb, prog) = explicate_assign(
                var,
                *els,
                BasicBlock::new("", Tail::Goto(tail_name.clone())),
                prog,
            );
            let then_name = &gensym("then_bb");
            let else_name = &gensym("else_bb");
            let prog = prog.add_bb(BasicBlock::new(&then_name, then_bb.tail));
            let prog = prog.add_bb(BasicBlock::new(&else_name, else_bb.tail));
            explicate_ifpred(*cnd, then_name, else_name, prog)
        }
        RVarAnf::ExprK::Allocate(num, ty) => assign(Expr::Allocate(num, ty), bb, prog),
        RVarAnf::ExprK::Collect(bytes) => (
            BasicBlock::new("", Tail::Seq(Stmt::Collect(bytes), Box::new(bb.tail))),
            prog,
        ),
        RVarAnf::ExprK::GlobalVar(var) => assign(Expr::GlobalVar(var), bb, prog),
        RVarAnf::ExprK::TupleRef(a, i) => assign(Expr::TupleRef(a.into(), i), bb, prog),
        RVarAnf::ExprK::TupleSet(a, i, v) => {
            assign(Expr::TupleSet(a.into(), i, v.into()), bb, prog)
        }
    }
}

use std::collections::BTreeSet;
pub fn explicate_expr(e: RVarAnf::Expr) -> Program {
    RVar::gensym_reset();
    let (bb, prog) = explicate_tail(e, Program::new());
    let prog = prog.add_bb(BasicBlock::new("start", bb.tail));
    let mut names = BTreeSet::new();
    for node_idx in prog.cfg.node_indices() {
        assert_eq!(names.insert(&prog.cfg[node_idx].name), true);
    }
    prog
}

fn _cprog_print_globals(globals: &Vec<(String, Value)>) -> String {
    {
        let mut occurs = std::collections::HashSet::new();
        let all_unique = globals.iter().all(|(name, _)| occurs.insert(name));
        assert!(all_unique);
    }
    let cstr = globals
        .iter()
        .map(|(name, value)| match value {
            Value::Int(i) => format!("int {} = {};\n", name, i),
            _ => unreachable!("unexpected value {:?}", value),
        })
        .collect();
    cstr
}

fn indent_string(indent: usize, s: &str) -> String {
    (0..indent)
        .into_iter()
        .map(|_| " ".to_string())
        .collect::<String>()
        + s
}
fn cprog_print_atom(a: &Atom) -> String {
    match a {
        Atom::Int(i) => format!("{}", i),
        Atom::Bool(b) => format!("{}", if *b { "true" } else { "false" }),
        Atom::Var(v) => format!("{}", v),
        Atom::Void => format!("/*void*/"),
        // Atom::Void => unimplemented!(),
    }
}
fn cprog_print_expr(expr: &Expr) -> String {
    let s: String = match expr {
        Expr::Atom(a) => cprog_print_atom(a),
        Expr::Read => "read_int()".to_string(),
        Expr::UnaryOp(UnaryOpKind::Not, a) => {
            format!("(!{})", cprog_print_atom(a))
        }
        Expr::UnaryOp(UnaryOpKind::Neg, a) => {
            format!("(-{})", cprog_print_atom(a))
        }
        Expr::BinaryOp(BinaryOpKind::Add, a1, a2) => {
            format!("({} + {})", cprog_print_atom(a1), cprog_print_atom(a2))
        }
        Expr::BinaryOp(BinaryOpKind::Lt, a1, a2) => {
            format!("({} < {})", cprog_print_atom(a1), cprog_print_atom(a2))
        }
        Expr::BinaryOp(BinaryOpKind::Eq, a1, a2) => {
            format!("({} == {})", cprog_print_atom(a1), cprog_print_atom(a2))
        }
        Expr::Allocate(1, ty @ Type::Tuple(..)) => {
            format!("(int64_t(free_ptr += {}))", type_size_in_bytes(&ty))
        }
        /*
        Expr::TupleSet(tu, idx, val) => {
            format!("/* tuple_set */")
        }
        Expr::TupleRef(tu, idx) => {
            format!("/* tuple_ref */")
        }
        */
        Expr::GlobalVar(label) => {
            format!("{}", label)
        }
        expr @ _ => unimplemented!("{:?}", expr),
    };
    s
}
fn cprog_print_stmt(indent: usize, stmt: &Stmt) -> String {
    let s: String = match stmt {
        Stmt::AssignVar(var, e) => {
            let sexpr = cprog_print_expr(e);
            indent_string(indent, &{
                if var != "_" {
                    format!("{} = {};", var, sexpr)
                } else {
                    sexpr
                }
            })
        }
        Stmt::Collect(_bytes) => indent_string(indent, &format!("/* collect */;")),
    };
    s + "\n"
}

type Vars = BTreeSet<String>;

fn cprog_print_tail(indent: usize, tail: &Tail) -> String {
    let tail: String = match tail {
        Tail::Return(e) => indent_string(indent, &format!("return {};", cprog_print_expr(e))),
        Tail::Seq(stmt, tail) => {
            let stmt = cprog_print_stmt(indent, stmt);
            stmt + &cprog_print_tail(indent, tail)
        }
        Tail::Goto(label) => indent_string(indent, &format!("goto {};", label)),
        Tail::IfStmt(pred, thn, els) => indent_string(
            indent,
            &format!(
                "if {} {{ {} }} else {{ {} }}",
                cprog_print_expr(pred),
                format!("goto {};", thn),
                format!("goto {};", els)
            ),
        ),
    };
    tail
}

pub fn print_cprog(prog: &Program) -> String {
    let indent = 4;
    let cfg = &prog.cfg;
    let main_func = cfg
        .node_indices()
        .map(|idx| &cfg[idx])
        .map(|bb| {
            let bb_name: String = format!("{}:\n", bb.name);
            let bb_tail = cprog_print_tail(indent, &bb.tail);
            bb_name + &bb_tail + "\n\n"
        })
        .collect::<String>();

    let mut sprog = String::new();
    sprog.push_str("#include <stdint.h>\n");
    sprog.push_str("#include \"runtime.h\"\n\n");
    //sprog.push_str(&cprog_print_globals(&globals));
    sprog.push_str("\nint main() {\n\n");
    sprog.push_str(
        &prog
            .local_vars
            .iter()
            .map(|(name, ty)| {
                indent_string(indent, &{
                    let s = match ty {
                        Type::Int => format!("int64_t {};", name),
                        Type::Bool => format!("bool {};", name),
                        Type::Tuple(..) => format!("int64_t {}; // tuple", name),
                        Type::Void => {
                            assert_eq!(name, "_");
                            format!("")
                        } //_ => panic!("unhandled typed {:?}", ty),
                    };
                    s + "\n"
                })
            })
            .collect::<String>(),
    );
    sprog.push_str("\n");
    sprog.push_str(&indent_string(indent, "goto start;\n\n"));
    sprog.push_str(&main_func);
    sprog.push_str("}");
    sprog
}
