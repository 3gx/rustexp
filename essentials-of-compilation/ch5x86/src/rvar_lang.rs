#[path = "./macros.rs"]
mod macros;
//use macros::bx;
use macros::r#match;

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

    Collect(Int),
    Allocate(Int, Type),
    GlobalVar(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr(pub TExpr<TypedExpr>, pub Type);

#[derive(Debug, Clone, PartialEq)]
pub struct Expr(pub TExpr<Expr>);

impl TExpr<Expr> {
    pub fn expr(self) -> Expr {
        Expr(self)
    }
}
impl TExpr<TypedExpr> {
    pub fn texpr(self, ty: Type) -> TypedExpr {
        TypedExpr(self, ty)
    }
}

impl Expr {
    pub fn bx(&self) -> Box<Expr> {
        Box::new(self.clone())
    }
    pub fn typed(self) -> TypedExpr {
        typed_expr(self)
    }
}
impl TypedExpr {
    pub fn bx(&self) -> Box<TypedExpr> {
        Box::new(self.clone())
    }
    pub fn untyped(self) -> Expr {
        untyped_expr(self)
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
    ((let [{ $($quote:tt)* } $body:tt] $tail:tt)) => {
        Expr(TExpr::Let(
            expr!{{ $($quote)* }},
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
    ((not $opnd:tt)) => {
        Expr(TExpr::UnaryOp(UnaryOpKind::Not, Box::new(expr!{$opnd}.into_term())))
    },
    ((add $lhs:tt $rhs:tt)) => {
        Expr(TExpr::BinaryOp(BinaryOpKind::Add,
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    ((or $lhs:tt $rhs:tt)) => {
        Expr(TExpr::BinaryOp(BinaryOpKind::Or,
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    ((and $lhs:tt $rhs:tt)) => {
        Expr(TExpr::BinaryOp(BinaryOpKind::And,
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
    ({ $($tt:tt)* }) => {
        $($tt)*
    },
    ((read)) => { Expr(TExpr::Read) },
    (true) => { Expr(TExpr::Bool(true)) },
    (false) => { Expr(TExpr::Bool(false)) },
    ($id:ident) => { stringify!($id) },
    ($e:expr) => { $e },
}

pub macro texpr {
    /*
    ((let [$id:ident $body:tt] $tail:tt)) => {
        Expr(TExpr::Let(
            stringify!($id).to_string(),
            Box::new(expr!{$body}.into_term()),
            Box::new(expr!{$tail}.into_term()),
        ))
    },
    */
    ((let $type:tt [_ $body:tt] $tail:tt)) => {
        TExpr::Let(
            stringify!(_).to_string(),
            Box::new(texpr!{$body}),
            Box::new(texpr!{$tail}),
        ).texpr(texpr!{$type})
    },
    ((let $type:tt [{ $($quote:tt)* } $body:tt] $tail:tt)) => {
        TExpr::Let(
            texpr!{{ $($quote)* }},
            Box::new(texpr!{$body}),
            Box::new(texpr!{$tail})
        ).texpr(texpr!{$type})
    },
    /*
    ((tuple $($expr:tt)*)) => {
        Expr(TExpr::Tuple(vec![$(expr!{$expr}.into_term()),*]))
    },
    */
    ((tupleset! $tu:tt $idx:tt $val:tt)) => {
        TExpr::TupleSet(Box::new(texpr!{$tu}), texpr!{$idx},
                        Box::new(texpr!{$val})).texpr(Type::Void)
    },
    /*
    ((tupleref $tu:tt $idx:tt)) => {
        Expr(TExpr::TupleRef(Box::new(expr!{$tu}.into_term()), expr!{$idx}))
    },
    ((neg $opnd:tt)) => {
        Expr(TExpr::UnaryOp(UnaryOpKind::Neg, Box::new(expr!{$opnd}.into_term())))
    },
    ((not $opnd:tt)) => {
        Expr(TExpr::UnaryOp(UnaryOpKind::Not, Box::new(expr!{$opnd}.into_term())))
    },
    */
    ((add $type:tt $lhs:tt $rhs:tt)) => {
        TExpr::BinaryOp(BinaryOpKind::Add,
                        Box::new(texpr!{$lhs}),
                        Box::new(texpr!{$rhs})).texpr(texpr!{$type})
    },
    /*
    ((or $lhs:tt $rhs:tt)) => {
        Expr(TExpr::BinaryOp(BinaryOpKind::Or,
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    ((and $lhs:tt $rhs:tt)) => {
        Expr(TExpr::BinaryOp(BinaryOpKind::And,
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    */
    ((lt $type:tt $lhs:tt $rhs:tt)) => {
        TExpr::BinaryOp(BinaryOpKind::CmpOp(CmpOpKind::Lt),
                        Box::new(texpr!{$lhs}),
                        Box::new(texpr!{$rhs})).texpr(texpr!{$type})
    },
    /*
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
    */
    ((if $type:tt $pred:tt $then:tt $else:tt)) => {
        TExpr::If(Box::new(texpr!{$pred}),
                  Box::new(texpr!{$then}),
                  Box::new(texpr!{$else})).texpr(texpr!{$type})
    },
    ((int $val:tt)) => {TExpr::Int(texpr!{$val} as Int).texpr(Type::Int)},
    ((var $type:tt $var:tt)) => {TExpr::Var(texpr!{$var}.to_string()).texpr(texpr!{$type})},
    ((gvar $type:tt $var:tt)) => {TExpr::GlobalVar(texpr!{$var}.to_string()).texpr(texpr!{$type})},
    ({ $($tt:tt)* }) => {
        $($tt)*
    },
    ((read)) => { TExpr::Read.texpr(Type::Int) },
    (void) => { TExpr::Void.texpr(Type::Void) },
    (true) => { TExpr::Bool(true).texpr(Type::Bool) },
    (false) => { TExpr::Bool(false).texpr(Type::Bool) },
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

use std::cell::RefCell;
use std::rc::Rc;
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    None,
    Int(Int),
    Bool(Bool),
    Void,
    Tuple(Vec<Value>),
    Heap(Rc<RefCell<Value>>),
}
impl Default for Value {
    fn default() -> Self {
        Value::None
    }
}
impl Value {
    pub fn int(self) -> Option<Int> {
        match self {
            Value::Int(n) => Some(n),
            Value::Bool(_) => None,
            Value::Void => None,
            Value::Tuple(..) => None,
            Value::Heap(what) => what.take().int(),
            Value::None => None,
        }
    }
    pub fn bool(self) -> Option<Bool> {
        match self {
            Value::Int(_) => None,
            Value::Bool(b) => Some(b),
            Value::Void => None,
            Value::Tuple(..) => None,
            Value::Heap(what) => what.take().clone().bool(),
            Value::None => None,
        }
    }
    pub fn tuple(self) -> Option<Vec<Value>> {
        match self {
            Value::Int(_) => None,
            Value::Bool(_) => None,
            Value::Void => None,
            Value::Tuple(vec) => Some(vec),
            Value::Heap(what) => what.take().clone().tuple(),
            Value::None => None,
        }
    }
    pub fn isvoid(&self) -> Bool {
        if let &Value::Void = self {
            true
        } else {
            false
        }
    }
    pub fn onheap(self) -> Value {
        Value::Heap(Rc::new(RefCell::new(self)))
    }
}
impl From<Int> for Value {
    fn from(item: Int) -> Self {
        Value::Int(item)
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

pub fn interp_impl<T: Clone>(env: &Env, e: &TExpr<T>, f: &impl Fn(&T) -> &TExpr<T>) -> Value {
    let interp = |env, e| interp_impl(env, f(e), f);
    use self::CmpOpKind as C;
    use {BinaryOpKind::*, UnaryOpKind::*};
    match e {
        TExpr::Int(n) => Value::Int(*n),
        TExpr::Bool(b) => Value::Bool(*b),
        TExpr::Void => Value::Void,
        TExpr::Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            Value::Int(input.trim().parse().unwrap())
        }
        TExpr::UnaryOp(Neg, e) => Value::Int(-interp(env, e).int().unwrap()),
        TExpr::BinaryOp(Add, e1, e2) => {
            Value::Int(interp(env, e1).int().unwrap() + interp(env, e2).int().unwrap())
        }
        TExpr::Var(x) => sym_get(env, &x).unwrap().clone(),
        TExpr::Let(x, e, body) => {
            let new_env = sym_set(env, x, &interp(env, e));
            interp(&new_env, body)
        }
        TExpr::If(cond, thn, els) => interp(
            env,
            if interp(env, cond).bool().unwrap() {
                thn
            } else {
                els
            },
        ),
        TExpr::BinaryOp(And, e1, e2) => {
            if interp(env, e1).bool().unwrap() {
                Value::Bool(interp(env, e2).bool().unwrap())
            } else {
                Value::Bool(false)
            }
        }
        TExpr::BinaryOp(Or, e1, e2) => {
            if interp(env, e1).bool().unwrap() {
                Value::Bool(true)
            } else {
                Value::Bool(interp(env, e2).bool().unwrap())
            }
        }
        TExpr::UnaryOp(Not, expr) => Value::Bool(!interp(env, expr).bool().unwrap()),
        TExpr::BinaryOp(BinaryOpKind::CmpOp(op), e1, e2) => {
            match (op, interp(env, e1), interp(env, e2)) {
                (C::Eq, Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
                (C::Eq, Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
                (C::Le, Value::Int(a), Value::Int(b)) => Value::Bool(a <= b),
                (C::Lt, Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                (C::Ge, Value::Int(a), Value::Int(b)) => Value::Bool(a >= b),
                (C::Gt, Value::Int(a), Value::Int(b)) => Value::Bool(a > b),
                x @ _ => panic!("type mismatch: {:?}", x),
            }
        }
        TExpr::Tuple(es) => {
            let val = es
                .into_iter()
                .map(|e| interp(env, e))
                .collect::<Vec<Value>>();
            Value::Heap(Rc::new(RefCell::new(Value::Tuple(val))))
        }
        TExpr::TupleSet(tu, idx, val) => {
            let tu = interp(env, tu);
            let val = interp(env, val);
            r#match! { [tu]
                Value::Heap(tu) if @{let Value::Tuple(tu) = &mut *tu.borrow_mut()} =>
                    tu[*idx as usize] = val,
                _ => panic!("internal error, expecting tuple but got {:?}", tu)
            }
            Value::Void
        }
        TExpr::TupleRef(tu, idx) => {
            let tu = interp(env, tu);
            r#match! { [tu]
                Value::Heap(tu) if @{let Value::Tuple(tu) = &*tu.borrow_mut()} =>
                    tu[*idx as usize].clone(),
                _ => panic!("internal error, expecting tuple but got {:?}", tu)
            }
        }
        TExpr::TupleLen(..) => unimplemented!(),
        TExpr::Allocate(..) => unimplemented!(),
        TExpr::Collect(..) => unimplemented!(),
        TExpr::GlobalVar(..) => unimplemented!(),
    }
}

pub fn interp_expr(Expr(e): &Expr) -> Value {
    interp_impl(&vec![], e, &|Expr(e)| e)
}

pub fn interp_texpr(TypedExpr(e, _): &TypedExpr) -> Value {
    interp_impl(&vec![], e, &|TypedExpr(e, _)| e)
}

pub fn interp_program(p: &Program) -> Value {
    match p {
        Program(e) => interp_expr(e),
    }
}

use std::collections::HashMap;

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
pub fn uniquify_expr(umap: &UMap, Expr(expr): Expr) -> Expr {
    use TExpr::*;
    let e = match expr {
        Var(x) => Var(sym_get(umap, &x).unwrap().clone()),
        Int(n) => Int(n),
        Bool(b) => Bool(b),
        Let(x, e, body) => {
            let newvar = gensym(&x);
            let umap = sym_set(umap, &x, &newvar);
            Let(
                newvar,
                uniquify_expr(&umap, *e).bx(),
                uniquify_expr(&umap, *body).bx(),
            )
        }
        UnaryOp(op, expr) => UnaryOp(op, uniquify_expr(umap, *expr).bx()),
        BinaryOp(op, e1, e2) => BinaryOp(
            op,
            uniquify_expr(umap, *e1).bx(),
            uniquify_expr(umap, *e2).bx(),
        ),
        Read => Read,
        If(e1, e2, e3) => If(
            uniquify_expr(umap, *e1).bx(),
            uniquify_expr(umap, *e2).bx(),
            uniquify_expr(umap, *e3).bx(),
        ),
        Tuple(..) => unimplemented!(),
        TupleLen(..) => unimplemented!(),
        TupleRef(..) => unimplemented!(),
        TupleSet(..) => unimplemented!(),
        Void => unimplemented!(),
        TExpr::Collect(..) => unimplemented!(),
        TExpr::Allocate(..) => unimplemented!(),
        TExpr::GlobalVar(..) => unimplemented!(),
    };
    Expr(e)
}

pub fn uniquify(p: Program) -> Program {
    match p {
        Program(e) => Program(uniquify_expr(&sym![], e)),
    }
}

pub type Ctx = SymTable<Type>;
pub fn typed_expr_ctx(ctx: &Ctx, Expr(expr): Expr) -> TypedExpr {
    //    r#match! { [expr]
    match expr {
        TExpr::Int(i) => TExpr::Int(i).texpr(Type::Int),
        TExpr::Bool(b) => TExpr::Bool(b).texpr(Type::Bool),
        TExpr::Void => TExpr::Void.texpr(Type::Void),
        TExpr::Var(x) => {
            let ty = sym_get(ctx, &x);
            let ty = match ty {
                Some(ty) => ty.clone(),
                None => panic!("unknown var {}", x),
            };
            TExpr::Var(x).texpr(ty)
        }
        TExpr::Let(x, expr, body) => {
            let TypedExpr(expr, expr_ty) = typed_expr_ctx(ctx, *expr);
            let ctx = sym_set(ctx, &x, &expr_ty);
            let TypedExpr(body, body_ty) = typed_expr_ctx(&ctx, *body);
            TExpr::Let(
                x,
                TypedExpr(expr, expr_ty).bx(),
                TypedExpr(body, body_ty.clone()).bx(),
            )
            .texpr(body_ty)
        }
        TExpr::If(pred, then_, else_) => {
            let TypedExpr(pred, pred_ty) = typed_expr_ctx(ctx, *pred);
            match &pred_ty {
                Type::Bool => (),
                x @ _ => panic!("type({:?}) must be Bool, got {:?}", pred, x),
            };
            let TypedExpr(then_, then_ty) = typed_expr_ctx(ctx, *then_);
            let TypedExpr(else_, else_ty) = typed_expr_ctx(ctx, *else_);
            if then_ty != else_ty {
                panic!(
                    "type({:?}) = {:?} != {:?} = type({:?})",
                    then_, then_ty, else_ty, else_
                )
            }
            let if_ty = then_ty.clone();
            TExpr::If(
                pred.texpr(pred_ty).bx(),
                then_.texpr(then_ty).bx(),
                else_.texpr(else_ty).bx(),
            )
            .texpr(if_ty)
        }
        TExpr::Read => TExpr::Read.texpr(Type::Int),
        TExpr::UnaryOp(op, expr) => match (op, typed_expr_ctx(ctx, *expr)) {
            (UnaryOpKind::Not, TypedExpr(e, Type::Bool)) => {
                TExpr::UnaryOp(op, e.texpr(Type::Bool).bx()).texpr(Type::Bool)
            }
            (UnaryOpKind::Neg, TypedExpr(e, Type::Int)) => {
                TExpr::UnaryOp(op, e.texpr(Type::Int).bx()).texpr(Type::Int)
            }
            x @ _ => panic!("unsupported {:?}", x),
        },
        TExpr::BinaryOp(op, e1, e2) => {
            match (op, typed_expr_ctx(ctx, *e1), typed_expr_ctx(ctx, *e2)) {
                (BinaryOpKind::Add, TypedExpr(e1, Type::Int), TypedExpr(e2, Type::Int)) => {
                    TExpr::BinaryOp(op, e1.texpr(Type::Int).bx(), e2.texpr(Type::Int).bx())
                        .texpr(Type::Int)
                }
                (
                    BinaryOpKind::And | BinaryOpKind::Or,
                    TypedExpr(e1, Type::Bool),
                    TypedExpr(e2, Type::Bool),
                ) => TExpr::BinaryOp(op, e1.texpr(Type::Bool).bx(), e2.texpr(Type::Bool).bx())
                    .texpr(Type::Bool),
                (
                    BinaryOpKind::CmpOp(CmpOpKind::Eq),
                    TypedExpr(e1, Type::Bool),
                    TypedExpr(e2, Type::Bool),
                ) => TExpr::BinaryOp(op, e1.texpr(Type::Bool).bx(), e2.texpr(Type::Bool).bx())
                    .texpr(Type::Bool),
                (BinaryOpKind::CmpOp(_), TypedExpr(e1, Type::Int), TypedExpr(e2, Type::Int)) => {
                    TExpr::BinaryOp(op, e1.texpr(Type::Int).bx(), e2.texpr(Type::Int).bx())
                        .texpr(Type::Bool)
                }
                x @ _ => panic!("type mismatch {:?}", x),
            }
        }
        TExpr::Tuple(es) => {
            let (es, ty) = es
                .into_iter()
                .map(|e| {
                    let TypedExpr(e, ty) = typed_expr_ctx(ctx, e);
                    (TypedExpr(e, ty.clone()), ty)
                })
                .unzip();
            TExpr::Tuple(es).texpr(Type::Tuple(ty))
        }
        TExpr::TupleRef(tu, idx) => {
            assert!(idx >= 0, "idx must be non-negative, got {}", idx);
            let TypedExpr(tu, tuty) = typed_expr_ctx(ctx, *tu);
            let elty = match &tuty {
                Type::Tuple(tys) => {
                    let idx = idx as usize;
                    assert!(
                        idx < tys.len(),
                        "violation: idx= {} < tuple_len= {}",
                        idx,
                        tys.len()
                    );
                    tys[idx].clone()
                }
                _ => panic!("type error, expecting tuple type, got {:?}", tuty),
            };
            TExpr::TupleRef(tu.texpr(tuty).bx(), idx).texpr(elty)
        }
        TExpr::TupleSet(tu, idx, val) => {
            assert!(idx >= 0, "idx must be non-negative, got {}", idx);
            let TypedExpr(tu, tuty) = typed_expr_ctx(ctx, *tu);
            let TypedExpr(val, valty) = typed_expr_ctx(ctx, *val);
            let elty = match &tuty {
                Type::Tuple(tys) => {
                    let idx = idx as usize;
                    assert!(
                        idx < tys.len(),
                        "violation: idx= {} < tuple_len= {}",
                        idx,
                        tys.len()
                    );
                    &tys[idx]
                }
                _ => panic!("type error, expecting tuple type, got {:?}", tuty),
            };
            assert_eq!(elty, &valty);
            TExpr::TupleSet(tu.texpr(tuty).bx(), idx, val.texpr(valty).bx()).texpr(Type::Void)
        }
        TExpr::Collect(bytes) => TExpr::Collect(bytes).texpr(Type::Void),
        TExpr::Allocate(1, ty @ Type::Tuple(..)) => TExpr::Allocate(1, ty.clone()).texpr(ty),
        x @ TExpr::Allocate(..) => panic!("unimplemented {:?}", x),
        TExpr::GlobalVar(x) => TExpr::GlobalVar(x).texpr(Type::Int),
        TExpr::TupleLen(..) => unimplemented!(),
    }
}

pub fn typed_expr(e: Expr) -> TypedExpr {
    typed_expr_ctx(&vec![], e)
}

fn untyped_expr(TypedExpr(expr, _): TypedExpr) -> Expr {
    match expr {
        TExpr::Int(i) => TExpr::Int(i).expr(),
        TExpr::Bool(b) => TExpr::Bool(b).expr(),
        TExpr::Void => TExpr::Void.expr(),
        TExpr::Var(x) => TExpr::Var(x).expr(),
        TExpr::Let(x, expr, body) => TExpr::Let(x, expr.untyped().bx(), body.untyped().bx()).expr(),
        TExpr::If(pred, then_, else_) => TExpr::If(
            pred.untyped().bx(),
            then_.untyped().bx(),
            else_.untyped().bx(),
        )
        .expr(),
        TExpr::Read => TExpr::Read.expr(),
        TExpr::UnaryOp(op, expr) => TExpr::UnaryOp(op, expr.untyped().bx()).expr(),
        TExpr::BinaryOp(op, e1, e2) => {
            TExpr::BinaryOp(op, e1.untyped().bx(), e2.untyped().bx()).expr()
        }
        TExpr::Tuple(es) => TExpr::Tuple(es.into_iter().map(|e| e.untyped()).collect()).expr(),
        TExpr::TupleRef(tu, idx) => TExpr::TupleRef(tu.untyped().bx(), idx).expr(),
        TExpr::TupleSet(tu, idx, val) => {
            TExpr::TupleSet(tu.untyped().bx(), idx, val.untyped().bx()).expr()
        }
        TExpr::TupleLen(..) => unimplemented!(),
        TExpr::Collect(..) => unimplemented!(),
        TExpr::Allocate(..) => unimplemented!(),
        TExpr::GlobalVar(..) => unimplemented!(),
    }
}
