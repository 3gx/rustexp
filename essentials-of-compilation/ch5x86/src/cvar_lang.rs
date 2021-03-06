#[path = "rvar_anf_lang.rs"]
pub mod rvar_anf_lang;
pub use rvar_anf_lang as RVarAnf;
pub use RVarAnf::rvar_lang as RVar;

#[path = "./macros.rs"]
mod macros;
use macros::r#match;

use RVar::{gensym, /*gensym_reset,*/ sym_get, sym_set, Env};
use RVarAnf::{interp_atom, Value};
pub use RVarAnf::{BinaryOpKind, Bool, Int, Type, UnaryOpKind};

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
pub struct BasicBlock(pub String, pub Tail);

#[derive(Debug, Clone)]
pub struct CProgram(pub Vec<BasicBlock>);

trait AppendBB {
    fn add_bb<'a>(&'a mut self, _: BasicBlock) -> String;
}

impl AppendBB for Vec<BasicBlock> {
    fn add_bb<'a>(&'a mut self, bb: BasicBlock) -> String {
        self.push(bb);
        self.last().unwrap().0.clone()
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

pub fn interp_tail(env: &Env, tail: &Tail, bbs: &BTreeMap<String, Tail>) -> Value {
    match tail {
        Tail::Return(exp) => interp_expr(env, exp),
        Tail::Seq(stmt, tail) => {
            let new_env = interp_stmt(env, stmt);
            interp_tail(&new_env, tail, bbs)
        }
        Tail::Goto(label) => interp_tail(env, bbs.get(label).unwrap(), bbs),
        Tail::IfStmt(pred, thn, els) => match interp_expr(env, pred) {
            Value::Bool(true) => interp_tail(env, bbs.get(thn).unwrap(), bbs),
            Value::Bool(false) => interp_tail(env, bbs.get(els).unwrap(), bbs),
            x @ _ => panic!("predicate must be Bool, got {:?}", x),
        },
    }
}

use std::collections::BTreeMap;
pub fn interp_prog(cprog: &CProgram) -> Value {
    let mut prog = BTreeMap::new();
    let CProgram(bbs) = cprog;
    for BasicBlock(name, tail) in bbs.clone() {
        prog.insert(name, tail);
    }
    let tail = prog.get(&"start".to_string()).unwrap();
    let env = [
        ("free_ptr".to_string(), Value::Int(0).onheap()),
        ("fromspace_end".to_string(), Value::Int(0).onheap()),
    ]
    .iter()
    .cloned()
    .collect();
    interp_tail(&env, tail, &prog)
}

fn explicate_ifpred(
    e: RVarAnf::Expr,
    then_name: &str,
    else_name: &str,
    bbs: Vec<BasicBlock>,
) -> (Tail, Vec<BasicBlock>) {
    match e {
        RVarAnf::Expr::UnaryOp(UnaryOpKind::Not, a) => (
            Tail::IfStmt(
                Expr::UnaryOp(UnaryOpKind::Not, a.into()),
                then_name.to_string(),
                else_name.to_string(),
            ),
            bbs,
        ),
        RVarAnf::Expr::BinaryOp(cmp, a1, a2)
            if cmp == BinaryOpKind::Eq || cmp == BinaryOpKind::Lt =>
        {
            (
                Tail::IfStmt(
                    Expr::BinaryOp(cmp, a1.into(), a2.into()),
                    then_name.to_string(),
                    else_name.to_string(),
                ),
                bbs,
            )
        }
        RVarAnf::Expr::Atom(RVarAnf::Atom::Bool(pred)) => (
            Tail::Goto(if pred {
                then_name.to_string()
            } else {
                else_name.to_string()
            }),
            bbs,
        ),
        RVarAnf::Expr::Atom(a @ RVarAnf::Atom::Var(_)) => (
            Tail::IfStmt(
                Expr::Atom(a.into()),
                then_name.to_string(),
                else_name.to_string(),
            ),
            bbs,
        ),
        RVarAnf::Expr::If(p_expr, t_expr, e_expr) => {
            let (then_tail, bbs) = explicate_ifpred(*t_expr, then_name, else_name, bbs);
            let (else_tail, mut bbs) = explicate_ifpred(*e_expr, then_name, else_name, bbs);
            let then_name = &bbs.add_bb(BasicBlock(gensym("then_bb"), then_tail)).clone();
            let else_name = &bbs.add_bb(BasicBlock(gensym("else_bb"), else_tail)).clone();
            explicate_ifpred(*p_expr, then_name, else_name, bbs)
        }
        RVarAnf::Expr::Let(x, expr, body) => {
            let (if_tail, bbs) = explicate_ifpred(*body, then_name, else_name, bbs);
            explicate_assign(&x, *expr, if_tail, bbs)
        }
        RVarAnf::Expr::TupleRef(a, i) => (
            Tail::IfStmt(
                Expr::TupleRef(a.into(), i),
                then_name.to_string(),
                else_name.to_string(),
            ),
            bbs,
        ),
        x @ _ => panic!("invalid 'if' predicate= {:?}", x),
    }
}

fn explicate_tail(e: RVarAnf::Expr, bbs: Vec<BasicBlock>) -> (Tail, Vec<BasicBlock>) {
    let ret = |e, bbs| (Tail::Return(e), bbs);
    match e {
        RVarAnf::Expr::Atom(a) => ret(Expr::Atom(a.into()), bbs),
        RVarAnf::Expr::Read => ret(Expr::Read, bbs),
        RVarAnf::Expr::UnaryOp(op, a) => ret(Expr::UnaryOp(op, a.into()), bbs),
        RVarAnf::Expr::BinaryOp(op, a1, a2) => ret(Expr::BinaryOp(op, a1.into(), a2.into()), bbs),
        RVarAnf::Expr::Let(x, expr, body) => {
            let (tail, bbs) = explicate_tail(*body, bbs);
            explicate_assign(&x, *expr, tail, bbs)
        }
        RVarAnf::Expr::If(cnd, thn, els) => {
            let (then_bb, bbs) = explicate_tail(*thn, bbs);
            let (else_bb, mut bbs) = explicate_tail(*els, bbs);
            let then_name = &gensym("then_bb");
            let else_name = &gensym("else_bb");
            bbs.push(BasicBlock(then_name.clone(), then_bb));
            bbs.push(BasicBlock(else_name.clone(), else_bb));
            explicate_ifpred(*cnd, then_name, else_name, bbs)
        }
        RVarAnf::Expr::Allocate(..) => unimplemented!(),
        RVarAnf::Expr::Collect(..) => unimplemented!(),
        RVarAnf::Expr::GlobalVar(..) => unimplemented!(),
        RVarAnf::Expr::TupleRef(a, i) => ret(Expr::TupleRef(a.into(), i), bbs),
        RVarAnf::Expr::TupleSet(..) => unimplemented!(),
    }
}

fn explicate_assign(
    var: &str,
    e: RVarAnf::Expr,
    tail: Tail,
    bbs: Vec<BasicBlock>,
) -> (Tail, Vec<BasicBlock>) {
    let assign = |e, tail, bbs| {
        (
            Tail::Seq(Stmt::AssignVar(var.to_string(), e), Box::new(tail)),
            bbs,
        )
    };
    match e {
        RVarAnf::Expr::Atom(a) => assign(Expr::Atom(a.into()), tail, bbs),
        RVarAnf::Expr::Read => assign(Expr::Read, tail, bbs),
        RVarAnf::Expr::UnaryOp(op, a) => assign(Expr::UnaryOp(op, a.into()), tail, bbs),
        RVarAnf::Expr::BinaryOp(op, a1, a2) => {
            assign(Expr::BinaryOp(op, a1.into(), a2.into()), tail, bbs)
        }
        RVarAnf::Expr::Let(x, expr, body) => {
            let (tail, bbs) = explicate_assign(var, *body, tail, bbs);
            explicate_assign(&x, *expr, tail, bbs)
        }
        RVarAnf::Expr::If(cnd, thn, els) => {
            let mut bbs = bbs;
            let tail_name = &gensym("tail_bb");
            bbs.push(BasicBlock(tail_name.clone(), tail));
            let (then_bb, bbs) = explicate_assign(var, *thn, Tail::Goto(tail_name.clone()), bbs);
            let (else_bb, mut bbs) =
                explicate_assign(var, *els, Tail::Goto(tail_name.clone()), bbs);
            let then_name = &gensym("then_bb");
            let else_name = &gensym("else_bb");
            bbs.push(BasicBlock(then_name.clone(), then_bb));
            bbs.push(BasicBlock(else_name.clone(), else_bb));
            explicate_ifpred(*cnd, then_name, else_name, bbs)
        }
        RVarAnf::Expr::Allocate(num, ty) => assign(Expr::Allocate(num, ty), tail, bbs),
        RVarAnf::Expr::Collect(bytes) => (Tail::Seq(Stmt::Collect(bytes), Box::new(tail)), bbs),
        RVarAnf::Expr::GlobalVar(var) => assign(Expr::GlobalVar(var), tail, bbs),
        RVarAnf::Expr::TupleRef(a, i) => assign(Expr::TupleRef(a.into(), i), tail, bbs),
        RVarAnf::Expr::TupleSet(a, i, v) => {
            assign(Expr::TupleSet(a.into(), i, v.into()), tail, bbs)
        }
    }
}

use std::collections::BTreeSet;
pub fn explicate_expr(e: RVarAnf::Expr) -> CProgram {
    RVar::gensym_reset();
    let (tail, mut bbs) = explicate_tail(e, vec![]);
    bbs.push(BasicBlock("start".to_string(), tail));
    let mut names = BTreeSet::new();
    for BasicBlock(name, _) in &bbs {
        assert_eq!(names.insert(name), true);
    }
    CProgram(bbs)
}

fn cprog_print_globals(globals: &Vec<(String, Value)>) -> String {
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
        Atom::Void => format!("void"),
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
        _ => "".to_string(),
    };
    s
}
fn cprog_print_stmt(indent: usize, stmt: &Stmt, vars: &mut Vars) -> String {
    let s: String = match stmt {
        Stmt::AssignVar(var, e) => {
            vars.insert(var.clone());
            indent_string(indent, &format!("{} = {};", var, cprog_print_expr(e)))
        }
        _ => format!("(void)42;"),
    };
    s + "\n"
}

type Vars = BTreeSet<String>;

fn cprog_print_tail(indent: usize, tail: &Tail, vars: &mut Vars) -> String {
    let tail: String = match tail {
        Tail::Return(e) => indent_string(indent, &format!("return {};", cprog_print_expr(e))),
        Tail::Seq(stmt, tail) => {
            let stmt = cprog_print_stmt(indent, stmt, vars);
            stmt + &cprog_print_tail(indent, tail, vars)
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

pub fn print_cprog(cprog: &CProgram) -> String {
    let globals = [
        ("free_ptr".to_string(), Value::Int(0)),
        ("fromspace_end".to_string(), Value::Int(0)),
    ]
    .iter()
    .cloned()
    .collect::<Vec<_>>();

    let mut vars: Vars = vec![].into_iter().collect();

    let indent = 4;
    let CProgram(bbs) = cprog;
    let main_func = bbs
        .iter()
        .map(|BasicBlock(name, tail)| {
            let bb_name: String = format!("{}:\n", name);
            let bb_tail = cprog_print_tail(indent, tail, &mut vars);
            bb_name + &bb_tail + "\n\n"
        })
        .collect::<String>();

    let mut prog = cprog_print_globals(&globals);
    prog.push_str("\nint main() {\n\n");
    prog.push_str(
        &vars
            .iter()
            .map(|x| indent_string(indent, &format!("int {} = 0;\n", x)))
            .collect::<String>(),
    );
    prog.push_str("\n");
    prog.push_str(&indent_string(indent, "goto start;\n\n"));
    prog.push_str(&main_func);
    prog.push_str("}");
    prog
}
