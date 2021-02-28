#[path = "./macros.rs"]
mod macros;
use macros::bx;

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum CmpOpKind {
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum UnaryOpKind {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum BinaryOpKind {
    CmpOp(CmpOpKind),
    Add,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    Void,
    Tuple(Vec<Type>),
}

type Int = i64;
type Bool = bool;

/*
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // atoms
    Int(Int),
    Bool(Bool),
    Var(String),

    // let expr
    Let(String, Box<Expr>, Box<Expr>),

    //control-flow
    If(Box<Expr>, Box<Expr>, Box<Expr>),

    Read,

    UnaryOp(UnaryOpKind, Box<Expr>),
    BinaryOp(BinaryOpKind, Box<Expr>, Box<Expr>),

    Tuple(Vec<Expr>),
    TupleLen(Box<Expr>),
    TupleRef(Box<Expr>, Int),
    TupleSet(Box<Expr>, Int, Box<Expr>),
    Void,
    HasType(Box<Expr>, Type),
}
*/

#[derive(Debug, Clone, PartialEq)]
pub enum TExpr<Expr: Clone> {
    // atoms
    Int(Int),
    Bool(Bool),
    Var(String),

    // let expr
    Let(String, Box<Expr>, Box<Expr>),

    //control-flow
    If(Box<Expr>, Box<Expr>, Box<Expr>),

    Read,

    UnaryOp(UnaryOpKind, Box<Expr>),
    BinaryOp(BinaryOpKind, Box<Expr>, Box<Expr>),

    Tuple(Vec<Expr>),
    TupleLen(Box<Expr>),
    TupleRef(Box<Expr>, Int),
    TupleSet(Box<Expr>, Int, Box<Expr>),
    Void,
    HasType(Box<Expr>, Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr(pub TExpr<TypedExpr>, pub Type);

#[derive(Debug, Clone, PartialEq)]
pub struct Expr(pub TExpr<Expr>);

impl TExpr<Expr> {
    pub fn expr(self) -> Expr {
        Expr(self)
    }
    pub fn bx(&self) -> Box<Expr> {
        Box::new(self.clone().expr())
    }
}

impl Expr {
    pub fn bx(&self) -> Box<Expr> {
        Box::new(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program(pub Expr);

// ---------------------------------------------------------------------------
// macro-base DSL

pub macro int($e:expr) {
    Expr(TExpr::Int($e))
}
pub macro bool($b:expr) {
    Expr(TExpr::Bool($b))
}

pub macro program($e:expr) {
    Program($e.into_term())
}

pub macro expr {
    ((let [$id:ident $body:tt] $tail:tt)) => {
        Expr(TExpr::Let(
            stringify!($id).to_string(),
            Box::new(expr!{$body}.into_term()),
            Box::new(expr!{$tail}.into_term()),
        ))
    },
    ((let [_ $body:tt] $tail:tt)) => {
        Expr(TExpr::Let(
            stringify!(_).to_string(),
            Box::new(expr!{$body}.into_term()),
            Box::new(expr!{$tail}.into_term()),
        ))
    },
    ((tuple $($expr:tt)*)) => {
        Expr(TExpr::Tuple(vec![$(expr!{$expr}.into_term()),*]))
    },
    ((tupleset! $tu:tt $idx:tt $val:tt)) => {
        Expr(TExpr::TupleSet(Box::new(expr!{$tu}.into_term()), expr!{$idx},
                       Box::new(expr!{$val}.into_term())))
    },
    ((tupleref $tu:tt $idx:tt)) => {
        Expr(TExpr::TupleRef(Box::new(expr!{$tu}.into_term()), expr!{$idx}))
    },
    ((neg $opnd:tt)) => {
        Expr(TExpr::UnaryOp(UnaryOpKind::Neg, Box::new(expr!{$opnd}.into_term())))
    },
    ((add $lhs:tt $rhs:tt)) => {
        Expr(TExpr::BinaryOp(BinaryOpKind::Add,
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    ((lt $lhs:tt $rhs:tt)) => {
        Expr(TExpr::BinaryOp(BinaryOpKind::CmpOp(CmpOpKind::Lt),
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    ((eq $lhs:tt $rhs:tt)) => {
        Expr(TExpr::BinaryOp(BinaryOpKind::CmpOp(CmpOpKind::Eq),
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    ((le $lhs:tt $rhs:tt)) => {
        Expr(TExpr::BinaryOp(BinaryOpKind::CmpOp(CmpOpKind::Le),
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    ((if $pred:tt $then:tt $else:tt)) => {
        Expr(TExpr::If(Box::new(expr!{$pred}.into_term()),
                 Box::new(expr!{$then}.into_term()),
                 Box::new(expr!{$else}.into_term())))
    },
    ((read)) => { Expr(TExpr::Read) },
    (true) => { Expr(TExpr::Bool(true)) },
    (false) => { Expr(TExpr::Bool(false)) },
    ($id:ident) => { stringify!($id) },
    ($e:expr) => { $e },
}

// ---------------------------------------------------------------------------

pub trait IntoTerm {
    fn into_term(&self) -> Expr;
}

impl IntoTerm for Int {
    fn into_term(&self) -> Expr {
        int!(*self)
    }
}
impl IntoTerm for bool {
    fn into_term(&self) -> Expr {
        bool!(*self)
    }
}
impl IntoTerm for Expr {
    fn into_term(&self) -> Expr {
        self.clone()
    }
}
impl IntoTerm for &str {
    fn into_term(&self) -> Expr {
        Expr(TExpr::Var(self.to_string()))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Value {
    Int(Int),
    Bool(Bool),
}
impl Value {
    pub fn int(&self) -> Option<&Int> {
        match self {
            Value::Int(n) => Some(n),
            Value::Bool(_) => None,
        }
    }
    pub fn bool(&self) -> Option<&Bool> {
        match self {
            Value::Int(_) => None,
            Value::Bool(b) => Some(b),
        }
    }
}

type SymTable<T> = Vec<(String, T)>;
pub type Env = SymTable<Value>;

pub macro sym {
    () => {
        Vec::new()
    },
    ($elem:expr; $n:expr) => {
        vec::from_elem($elem, $n)
    },
    ($($x:expr),+ $(,)?) => {
        <[_]>::into_vec(Box::new([$($x),+]))
    },
}

pub fn sym_get<'a, T>(sym: &'a SymTable<T>, key: &str) -> Option<&'a T> {
    sym.iter()
        .rev()
        .find_map(|x| if x.0 != key { None } else { Some(&x.1) })
}

pub fn sym_set<T: Clone>(sym: &SymTable<T>, key: &str, val: &T) -> SymTable<T> {
    let mut sym = sym.clone();
    sym.push((key.to_string(), val.clone()));
    sym
}

pub fn interp_exp(env: &Env, Expr(e): &Expr) -> Value {
    use self::CmpOpKind as C;
    use {BinaryOpKind::*, UnaryOpKind::*};
    match e {
        TExpr::Int(n) => Value::Int(*n),
        TExpr::Bool(b) => Value::Bool(*b),
        TExpr::Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            Value::Int(input.trim().parse().unwrap())
        }
        TExpr::UnaryOp(Neg, e) => Value::Int(-interp_exp(env, e).int().unwrap()),
        TExpr::BinaryOp(Add, e1, e2) => {
            Value::Int(interp_exp(env, e1).int().unwrap() + interp_exp(env, e2).int().unwrap())
        }
        TExpr::Var(x) => sym_get(env, &x).unwrap().clone(),
        TExpr::Let(x, e, body) => {
            let new_env = sym_set(env, x, &interp_exp(env, e));
            interp_exp(&new_env, body)
        }
        TExpr::If(cond, thn, els) => interp_exp(
            env,
            if *interp_exp(env, cond).bool().unwrap() {
                thn
            } else {
                els
            },
        ),
        TExpr::BinaryOp(And, e1, e2) => {
            if *interp_exp(env, e1).bool().unwrap() {
                Value::Bool(*interp_exp(env, e2).bool().unwrap())
            } else {
                Value::Bool(false)
            }
        }
        TExpr::BinaryOp(Or, e1, e2) => {
            if *interp_exp(env, e1).bool().unwrap() {
                Value::Bool(true)
            } else {
                Value::Bool(*interp_exp(env, e2).bool().unwrap())
            }
        }
        TExpr::UnaryOp(Not, expr) => Value::Bool(!interp_exp(env, expr).bool().unwrap()),
        TExpr::BinaryOp(BinaryOpKind::CmpOp(op), e1, e2) => {
            match (op, interp_exp(env, e1), interp_exp(env, e2)) {
                (C::Eq, Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
                (C::Eq, Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
                (C::Le, Value::Int(a), Value::Int(b)) => Value::Bool(a <= b),
                (C::Lt, Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                (C::Ge, Value::Int(a), Value::Int(b)) => Value::Bool(a >= b),
                (C::Gt, Value::Int(a), Value::Int(b)) => Value::Bool(a > b),
                x @ _ => panic!("type mismatch: {:?}", x),
            }
        }
        TExpr::Tuple(..) => unimplemented!(),
        TExpr::TupleLen(..) => unimplemented!(),
        TExpr::TupleRef(..) => unimplemented!(),
        TExpr::TupleSet(..) => unimplemented!(),
        TExpr::Void => unimplemented!(),
        TExpr::HasType(..) => unimplemented!(),
    }
}

pub fn interp_program(p: &Program) -> Value {
    match p {
        Program(e) => interp_exp(&vec![], e),
    }
}

use std::collections::HashMap;

use std::cell::RefCell;
thread_local! {
    static MAP: RefCell<HashMap<String, usize>> = RefCell::new(HashMap::new());
}
pub fn gensym_reset() {
    MAP.with(|map| map.borrow_mut().clear());
}
pub fn gensym(x: &str) -> String {
    let counter = MAP.with(|map| {
        let mut map = map.borrow_mut();
        let counter = map.entry(x.to_string()).or_insert(0);
        *counter += 1;
        *counter
    });
    x.to_string() + &counter.to_string()
}

type UMap = SymTable<String>;
pub fn uniquify_expr(umap: &UMap, Expr(expr): &Expr) -> Expr {
    use TExpr::*;
    let e = match expr {
        Var(x) => Var(sym_get(umap, x).unwrap().clone()),
        Int(n) => Int(*n),
        Bool(b) => Bool(*b),
        Let(x, e, body) => {
            let newvar = gensym(x);
            let umap = sym_set(umap, x, &newvar);
            Let(
                newvar,
                bx![uniquify_expr(&umap, e)],
                bx![uniquify_expr(&umap, body)],
            )
        }
        UnaryOp(op, expr) => UnaryOp(*op, bx![uniquify_expr(umap, expr)]),
        BinaryOp(op, e1, e2) => BinaryOp(
            *op,
            bx![uniquify_expr(umap, e1)],
            bx![uniquify_expr(umap, e2)],
        ),
        Read => Read,
        If(e1, e2, e3) => If(
            bx![uniquify_expr(umap, e1)],
            bx![uniquify_expr(umap, e2)],
            bx![uniquify_expr(umap, e3)],
        ),
        Tuple(..) => unimplemented!(),
        TupleLen(..) => unimplemented!(),
        TupleRef(..) => unimplemented!(),
        TupleSet(..) => unimplemented!(),
        Void => unimplemented!(),
        HasType(..) => unimplemented!(),
    };
    Expr(e)
}

pub fn uniquify(p: Program) -> Program {
    match p {
        Program(e) => Program(uniquify_expr(&sym![], &e)),
    }
}

pub type Ctx = SymTable<Type>;
pub fn type_expr(ctx: &Ctx, Expr(expr): &Expr) -> Type {
    match expr {
        TExpr::Int(_) => Type::Int,
        TExpr::Bool(_) => Type::Bool,
        TExpr::Var(x) => sym_get(ctx, x).unwrap().clone(),
        TExpr::Let(x, expr, body) => {
            let ty = type_expr(ctx, expr);
            let ctx = sym_set(ctx, x, &ty);
            type_expr(&ctx, body)
        }
        TExpr::If(pred, then_, else_) => {
            match type_expr(ctx, pred) {
                Type::Bool => (),
                x @ _ => panic!("type({:?}) must be Bool, got {:?}", pred, x),
            };
            let then_ty = type_expr(ctx, then_);
            let else_ty = type_expr(ctx, else_);
            if then_ty != else_ty {
                panic!(
                    "type({:?}) = {:?} != {:?} = type({:?})",
                    then_, then_ty, else_ty, else_
                )
            }
            then_ty
        }
        TExpr::Read => Type::Int,
        TExpr::UnaryOp(op, expr) => match (op, type_expr(ctx, expr)) {
            (UnaryOpKind::Not, Type::Bool) => Type::Bool,
            (UnaryOpKind::Neg, Type::Int) => Type::Int,
            x @ _ => panic!("unsupported {:?}", x),
        },
        TExpr::BinaryOp(op, e1, e2) => match (op, type_expr(ctx, e1), type_expr(ctx, e2)) {
            (BinaryOpKind::Add, Type::Int, Type::Int) => Type::Int,
            (BinaryOpKind::And, Type::Bool, Type::Bool) => Type::Bool,
            (BinaryOpKind::Or, Type::Bool, Type::Bool) => Type::Bool,
            (BinaryOpKind::CmpOp(CmpOpKind::Eq), Type::Bool, Type::Bool) => Type::Bool,
            (BinaryOpKind::CmpOp(_), Type::Int, Type::Int) => Type::Bool,
            x @ _ => panic!("unsupported {:?}", x),
        },
        TExpr::Tuple(..) => unimplemented!(),
        TExpr::TupleLen(..) => unimplemented!(),
        TExpr::TupleRef(..) => unimplemented!(),
        TExpr::TupleSet(..) => unimplemented!(),
        TExpr::Void => unimplemented!(),
        TExpr::HasType(..) => unimplemented!(),
    }
}
