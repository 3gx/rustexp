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
    Fun(Vec<Type>, Box<Type>),
}

type Int = i64;
type Bool = bool;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprK {
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

    Apply(Box<Expr>, Vec<Expr>),

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
pub struct Expr(pub ExprK, pub Option<Type>);

impl ExprK {
    pub fn expr(self) -> Expr {
        Expr(self, None)
    }
    pub fn typed(self, ty: Type) -> Expr {
        Expr(self, Some(ty))
    }
}

impl Expr {
    pub fn rbx(&self) -> Box<Expr> {
        Box::new(self.clone())
    }
    pub fn bx(self) -> Box<Expr> {
        Box::new(self)
    }
    pub fn is_typed(&self) -> Bool {
        !self.1.is_none()
    }
    pub fn typed(mut self, ty: Type) -> Expr {
        assert!(!self.is_typed());
        self.1 = Some(ty);
        self
    }
    pub fn untyped(mut self) -> Expr {
        assert!(self.is_typed());
        self.1 = None;
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefFun(pub String, pub Vec<(String, Type)>, pub Type, pub Expr);

#[derive(Debug, Clone, PartialEq)]
pub struct Program(pub Vec<DefFun>, pub Expr);

// ---------------------------------------------------------------------------
// macro-base DSL

pub macro int($e:expr) {
    Expr(ExprK::Int($e), None)
}
pub macro bool($b:expr) {
    Expr(ExprK::Bool($b), None)
}

pub macro program($e:expr) {
    Program(vec![], $e.into_term())
}
pub macro mktype {
    (Int) => {Type::Int},
    (Bool) => {Type::Bool},
    (Void) => {Type::Void},
    ((-> $ty1:tt $($rest:tt)*)) => { {
           let mut types = vec![mktype!($ty1), $(mktype!($rest)),*];
           let retty = types.pop().unwrap();
           Type::Fun(types,  Box::new(retty))
        }
   //     Type::Fun(vec![$(mktype!($argty)),*], Box::new(mktype!($retty)))
    },
    ((Tuple $($types:tt)*)) => {Type::Tuple(vec![$(mktype!($types)),*])},
    //($($tt:tt)*) => {Type::Void},
}

pub macro mkfun(($name:ident $([$var:ident : $($varty:tt)*])*), $retty:tt, $body:tt) {
    DefFun(stringify!($name).to_string(),
           vec![$((stringify!($var).to_string(), mktype!($($varty)*))),*],
           mktype!($retty),
           expr!{$body})
}
pub macro program1impl {
    ((@prog (defun $params:tt -> $retty:tt $funbody:tt) $($tail:tt)*)
     (@funs $($funs:tt)*)) => {
        program1impl!((@prog $($tail)*)
                      (@funs $($funs)* mkfun!($params, $retty, $funbody),))
    },
    ((@prog $main:tt)
     (@funs $($funs:tt)*)) => {
        Program(vec![$($funs)*], expr!{$main})
    },
}
pub macro program1 {
    ($($tt:tt)*) => {program1impl!((@prog $($tt)*) (@funs))},
}

pub macro expr {
    ((let [$id:ident $body:tt] $tail:tt)) => {
        ExprK::Let(
            stringify!($id).to_string(),
            Box::new(expr!{$body}.into_term()),
            Box::new(expr!{$tail}.into_term()),
        ).expr()
    },
    ((let [_ $body:tt] $tail:tt)) => {
        ExprK::Let(
            stringify!(_).to_string(),
            Box::new(expr!{$body}.into_term()),
            Box::new(expr!{$tail}.into_term()),
        ).expr()
    },
    ((let [{ $($quote:tt)* } $body:tt] $tail:tt)) => {
        ExprK::Let(
            expr!{{ $($quote)* }},
            Box::new(expr!{$body}.into_term()),
            Box::new(expr!{$tail}.into_term()),
        ).expr()
    },
    ((tuple $($expr:tt)*)) => {
        ExprK::Tuple(vec![$(expr!{$expr}.into_term()),*]).expr()
    },
    ((tupleset! $tu:tt $idx:tt $val:tt)) => {
        ExprK::TupleSet(Box::new(expr!{$tu}.into_term()), expr!{$idx},
                       Box::new(expr!{$val}.into_term())).expr()
    },
    ((tupleref $tu:tt $idx:tt)) => {
        ExprK::TupleRef(Box::new(expr!{$tu}.into_term()), expr!{$idx}).expr()
    },
    ((neg $opnd:tt)) => {
        ExprK::UnaryOp(UnaryOpKind::Neg, Box::new(expr!{$opnd}.into_term())).expr()
    },
    ((not $opnd:tt)) => {
        ExprK::UnaryOp(UnaryOpKind::Not, Box::new(expr!{$opnd}.into_term())).expr()
    },
    ((add $lhs:tt $rhs:tt)) => {
        ExprK::BinaryOp(BinaryOpKind::Add,
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())).expr()
    },
    ((or $lhs:tt $rhs:tt)) => {
        ExprK::BinaryOp(BinaryOpKind::Or,
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())).expr()
    },
    ((and $lhs:tt $rhs:tt)) => {
        ExprK::BinaryOp(BinaryOpKind::And,
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())).expr()
    },
    ((lt $lhs:tt $rhs:tt)) => {
        ExprK::BinaryOp(BinaryOpKind::CmpOp(CmpOpKind::Lt),
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())).expr()
    },
    ((eq $lhs:tt $rhs:tt)) => {
        ExprK::BinaryOp(BinaryOpKind::CmpOp(CmpOpKind::Eq),
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())).expr()
    },
    ((le $lhs:tt $rhs:tt)) => {
        ExprK::BinaryOp(BinaryOpKind::CmpOp(CmpOpKind::Le),
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())).expr()
    },
    ((if $pred:tt $then:tt $else:tt)) => {
        ExprK::If(Box::new(expr!{$pred}.into_term()),
                 Box::new(expr!{$then}.into_term()),
                 Box::new(expr!{$else}.into_term())).expr()
    },
    ((-$expr:tt)) => { ExprK::UnaryOp(UnaryOpKind::Neg,Box::new(expr!{$expr}.into_term())).expr() },
    ({ $($tt:tt)* }) => {
        $($tt)*
    },
    ((read)) => { ExprK::Read.expr() },
    (true) => { ExprK::Bool(true).expr() },
    (false) => { ExprK::Bool(false).expr() },
    ($id:ident) => { stringify!($id) },
    (($expr:tt $($args:tt)*)) => {ExprK::Apply(Box::new(expr!{$expr}.into_term()),
                                               vec![$(expr!{$args}.into_term()),*]).expr()},
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
        ExprK::Let(
            stringify!(_).to_string(),
            Box::new(texpr!{$body}),
            Box::new(texpr!{$tail}),
        ).typed(texpr!{$type})
    },
    ((let $type:tt [{ $($quote:tt)* } $body:tt] $tail:tt)) => {
        ExprK::Let(
            texpr!{{ $($quote)* }},
            Box::new(texpr!{$body}),
            Box::new(texpr!{$tail})
        ).typed(texpr!{$type})
    },
    /*
    ((tuple $($expr:tt)*)) => {
        Expr(ExprK::Tuple(vec![$(expr!{$expr}.into_term()),*]))
    },
    */
    ((tupleset! $tu:tt $idx:tt $val:tt)) => {
        ExprK::TupleSet(Box::new(texpr!{$tu}), texpr!{$idx},
                        Box::new(texpr!{$val})).typed(Type::Void)
    },
    /*
    ((tupleref $tu:tt $idx:tt)) => {
        Expr(ExprK::TupleRef(Box::new(expr!{$tu}.into_term()), expr!{$idx}))
    },
    ((neg $opnd:tt)) => {
        Expr(ExprK::UnaryOp(UnaryOpKind::Neg, Box::new(expr!{$opnd}.into_term())))
    },
    ((not $opnd:tt)) => {
        Expr(ExprK::UnaryOp(UnaryOpKind::Not, Box::new(expr!{$opnd}.into_term())))
    },
    */
    ((add $type:tt $lhs:tt $rhs:tt)) => {
        ExprK::BinaryOp(BinaryOpKind::Add,
                        Box::new(texpr!{$lhs}),
                        Box::new(texpr!{$rhs})).typed(texpr!{$type})
    },
    /*
    ((or $lhs:tt $rhs:tt)) => {
        Expr(ExprK::BinaryOp(BinaryOpKind::Or,
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    ((and $lhs:tt $rhs:tt)) => {
        Expr(ExprK::BinaryOp(BinaryOpKind::And,
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    */
    ((lt $type:tt $lhs:tt $rhs:tt)) => {
        ExprK::BinaryOp(BinaryOpKind::CmpOp(CmpOpKind::Lt),
                        Box::new(texpr!{$lhs}),
                        Box::new(texpr!{$rhs})).typed(texpr!{$type})
    },
    /*
    ((eq $lhs:tt $rhs:tt)) => {
        Expr(ExprK::BinaryOp(BinaryOpKind::CmpOp(CmpOpKind::Eq),
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    ((le $lhs:tt $rhs:tt)) => {
        Expr(ExprK::BinaryOp(BinaryOpKind::CmpOp(CmpOpKind::Le),
                       Box::new(expr!{$lhs}.into_term()),
                       Box::new(expr!{$rhs}.into_term())))
    },
    */
    ((if $type:tt $pred:tt $then:tt $else:tt)) => {
        ExprK::If(Box::new(texpr!{$pred}),
                  Box::new(texpr!{$then}),
                  Box::new(texpr!{$else})).typed(texpr!{$type})
    },
    ((int $val:tt)) => {ExprK::Int(texpr!{$val} as Int).typed(Type::Int)},
    ((var $type:tt $var:tt)) => {ExprK::Var(texpr!{$var}.to_string()).typed(texpr!{$type})},
    ((gvar $type:tt $var:tt)) => {ExprK::GlobalVar(texpr!{$var}.to_string()).typed(texpr!{$type})},
    ({ $($tt:tt)* }) => {
        $($tt)*
    },
    ((read)) => { ExprK::Read.typed(Type::Int) },
    (void) => { ExprK::Void.typed(Type::Void) },
    (true) => { ExprK::Bool(true).typed(Type::Bool) },
    (false) => { ExprK::Bool(false).typed(Type::Bool) },
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
        ExprK::Var(self.to_string()).expr()
    }
}

use std::cell::RefCell;
use std::rc::Rc;
#[derive(Debug, Clone, PartialEq)]
pub struct VFun(Env, Vec<String>, Expr);
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    None,
    Int(Int),
    Bool(Bool),
    Void,
    Tuple(Vec<Value>),
    Heap(Rc<RefCell<Value>>),
    Fun(VFun),
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
            Value::Heap(what) => what.take().int(),
            Value::Bool(_) => None,
            Value::Void => None,
            Value::Tuple(..) => None,
            Value::None => None,
            Value::Fun(..) => None,
        }
    }
    pub fn bool(self) -> Option<Bool> {
        match self {
            Value::Bool(b) => Some(b),
            Value::Heap(what) => what.take().clone().bool(),
            Value::Int(_) => None,
            Value::Void => None,
            Value::Tuple(..) => None,
            Value::None => None,
            Value::Fun(..) => None,
        }
    }
    pub fn tuple(self) -> Option<Vec<Value>> {
        match self {
            Value::Tuple(vec) => Some(vec),
            Value::Heap(what) => what.take().clone().tuple(),
            Value::Int(_) => None,
            Value::Bool(_) => None,
            Value::Void => None,
            Value::None => None,
            Value::Fun(..) => None,
        }
    }
    pub fn fun(self) -> Option<VFun> {
        match self {
            Value::Fun(vfun) => Some(vfun),
            Value::Tuple(_) => None,
            Value::Heap(_) => None,
            Value::Int(_) => None,
            Value::Bool(_) => None,
            Value::Void => None,
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

pub fn interp_impl(env: &Env, Expr(e, _): &Expr) -> Value {
    let interp = interp_impl;
    use self::CmpOpKind as C;
    use {BinaryOpKind::*, UnaryOpKind::*};
    match e {
        ExprK::Int(n) => Value::Int(*n),
        ExprK::Bool(b) => Value::Bool(*b),
        ExprK::Void => Value::Void,
        ExprK::Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            Value::Int(input.trim().parse().unwrap())
        }
        ExprK::UnaryOp(Neg, e) => Value::Int(-interp(env, e).int().unwrap()),
        ExprK::BinaryOp(Add, e1, e2) => {
            Value::Int(interp(env, e1).int().unwrap() + interp(env, e2).int().unwrap())
        }
        ExprK::Var(x) => match sym_get(env, &x) {
            Some(val) => val.clone(),
            _ => panic!("unknown variable {:?}", x),
        },
        ExprK::Let(x, e, body) => {
            let new_env = sym_set(env, x, &interp(env, e));
            interp(&new_env, body)
        }
        ExprK::If(cond, thn, els) => interp(
            env,
            if interp(env, cond).bool().unwrap() {
                thn
            } else {
                els
            },
        ),
        ExprK::BinaryOp(And, e1, e2) => {
            if interp(env, e1).bool().unwrap() {
                Value::Bool(interp(env, e2).bool().unwrap())
            } else {
                Value::Bool(false)
            }
        }
        ExprK::BinaryOp(Or, e1, e2) => {
            if interp(env, e1).bool().unwrap() {
                Value::Bool(true)
            } else {
                Value::Bool(interp(env, e2).bool().unwrap())
            }
        }
        ExprK::UnaryOp(Not, expr) => Value::Bool(!interp(env, expr).bool().unwrap()),
        ExprK::BinaryOp(BinaryOpKind::CmpOp(op), e1, e2) => {
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
        ExprK::Tuple(es) => {
            let val = es
                .into_iter()
                .map(|e| interp(env, e))
                .collect::<Vec<Value>>();
            Value::Heap(Rc::new(RefCell::new(Value::Tuple(val))))
        }
        ExprK::TupleSet(tu, idx, val) => {
            let tu = interp(env, tu);
            let val = interp(env, val);
            r#match! { [tu]
                Value::Heap(tu) if @{let Value::Tuple(tu) = &mut *tu.borrow_mut()} =>
                    tu[*idx as usize] = val,
                _ => panic!("internal error, expecting tuple but got {:?}", tu)
            }
            Value::Void
        }
        ExprK::TupleRef(tu, idx) => {
            let tu = interp(env, tu);
            r#match! { [tu]
                Value::Heap(tu) if @{let Value::Tuple(tu) = &*tu.borrow_mut()} =>
                    tu[*idx as usize].clone(),
                _ => panic!("internal error, expecting tuple but got {:?}", tu)
            }
        }
        ExprK::Apply(fun, args) => {
            let VFun(fun_env, vars, body) = interp(env, fun).fun().unwrap();
            if vars.len() != args.len() {
                panic!(
                    "argument mismatch for fun {:?}, expecting {}, got {}",
                    fun,
                    vars.len(),
                    args.len()
                );
            }

            let fun_env = {
                let mut fun_env = fun_env;
                for (var, arg) in vars.into_iter().zip(args.iter()) {
                    fun_env = sym_set(&fun_env, &var, &interp_impl(&env, arg));
                }
                fun_env
            };
            interp_impl(&fun_env, &body)
        }
        ExprK::TupleLen(..) => unimplemented!(),
        ExprK::Allocate(..) => unimplemented!(),
        ExprK::Collect(..) => unimplemented!(),
        ExprK::GlobalVar(..) => unimplemented!(),
    }
}

pub fn interp_expr(e: &Expr) -> Value {
    interp_impl(&vec![], e)
}
pub fn interp_texpr(e: &Expr) -> Value {
    assert!(e.is_typed());
    interp_impl(&vec![], e)
}

pub fn interp_program(Program(funs, main): &Program) -> Value {
    // first pass: create a global environment with functions
    let genv: Env = funs
        .iter()
        .cloned()
        .map(|DefFun(name, params, _, body)| {
            let vars = params.into_iter().map(|(name, _)| name).collect();
            (name, Value::Fun(VFun(Env::new(), vars, body)))
        })
        .collect();

    // update each function environment with the created global env
    let genv = {
        let cloned_genv = genv.clone();
        let mut env = genv;
        for (_, val) in &mut env {
            if let Value::Fun(VFun(env, _, _)) = val {
                *env = cloned_genv.clone();
            }
        }
        env
    };
    interp_impl(&genv, main)
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
pub fn uniquify_expr_impl(umap: &UMap, Expr(expr, ty): Expr) -> Expr {
    use ExprK::*;
    let e = match expr {
        Var(x) => Var(sym_get(umap, &x).unwrap().clone()),
        Int(n) => Int(n),
        Bool(b) => Bool(b),
        Let(x, e, body) => {
            let newvar = gensym(&x);
            let umap = sym_set(umap, &x, &newvar);
            Let(
                newvar,
                uniquify_expr_impl(&umap, *e).bx(),
                uniquify_expr_impl(&umap, *body).bx(),
            )
        }
        UnaryOp(op, expr) => UnaryOp(op, uniquify_expr_impl(umap, *expr).bx()),
        BinaryOp(op, e1, e2) => BinaryOp(
            op,
            uniquify_expr_impl(umap, *e1).bx(),
            uniquify_expr_impl(umap, *e2).bx(),
        ),
        Read => Read,
        If(e1, e2, e3) => If(
            uniquify_expr_impl(umap, *e1).bx(),
            uniquify_expr_impl(umap, *e2).bx(),
            uniquify_expr_impl(umap, *e3).bx(),
        ),
        Tuple(es) => Tuple(
            es.into_iter()
                .map(|e| uniquify_expr_impl(umap, e))
                .collect(),
        ),
        TupleLen(e) => TupleLen(uniquify_expr_impl(umap, *e).bx()),
        TupleRef(tu, idx) => TupleRef(uniquify_expr_impl(umap, *tu).bx(), idx),
        TupleSet(tu, idx, val) => TupleSet(
            uniquify_expr_impl(umap, *tu).bx(),
            idx,
            uniquify_expr_impl(umap, *val).bx(),
        ),
        Apply(fun, args) => Apply(
            uniquify_expr_impl(umap, *fun).bx(),
            args.into_iter()
                .map(|x| uniquify_expr_impl(umap, x))
                .collect(),
        ),
        Void => unimplemented!(),
        Collect(..) => unimplemented!(),
        Allocate(..) => unimplemented!(),
        GlobalVar(..) => unimplemented!(),
    };
    Expr(e, ty)
}

pub fn uniquify_expr(e: Expr) -> Expr {
    uniquify_expr_impl(&sym![], e)
}
pub fn uniquify(Program(funs, main): Program) -> Program {
    let main = uniquify_expr(main);
    Program(funs, main)
}

pub type Ctx = SymTable<Type>;
pub fn typed_expr_ctx(ctx: &Ctx, Expr(expr, ty): Expr) -> Expr {
    assert!(ty.is_none());
    use ExprK::*;
    //    r#match! { [expr]
    match expr {
        Int(i) => Int(i).typed(Type::Int),
        Bool(b) => Bool(b).typed(Type::Bool),
        Void => Void.typed(Type::Void),
        Var(x) => {
            let ty = sym_get(ctx, &x);
            let ty = match ty {
                Some(ty) => ty.clone(),
                None => panic!("unknown var {}", x),
            };
            Var(x).typed(ty)
        }
        Let(x, expr, body) => {
            let Expr(expr, expr_ty) = typed_expr_ctx(ctx, *expr);
            let ctx = sym_set(ctx, &x, &expr_ty.as_ref().unwrap());
            let Expr(body, body_ty) = typed_expr_ctx(&ctx, *body);
            Let(
                x,
                Expr(expr, expr_ty).bx(),
                Expr(body, body_ty.clone()).bx(),
            )
            .typed(body_ty.unwrap())
        }
        If(pred, then_, else_) => {
            let Expr(pred, pred_ty) = typed_expr_ctx(ctx, *pred);
            let pred_ty = pred_ty.unwrap();
            match &pred_ty {
                Type::Bool => (),
                x @ _ => panic!("type({:?}) must be Bool, got {:?}", pred, x),
            };
            let Expr(then_, then_ty) = typed_expr_ctx(ctx, *then_);
            let Expr(else_, else_ty) = typed_expr_ctx(ctx, *else_);
            if then_ty != else_ty {
                panic!(
                    "type({:?}) = {:?} != {:?} = type({:?})",
                    then_, then_ty, else_ty, else_
                )
            }
            let if_ty = then_ty.clone();
            If(
                pred.typed(pred_ty).bx(),
                then_.typed(then_ty.unwrap()).bx(),
                else_.typed(else_ty.unwrap()).bx(),
            )
            .typed(if_ty.unwrap())
        }
        Read => Read.typed(Type::Int),
        UnaryOp(op, expr) => match (op, typed_expr_ctx(ctx, *expr)) {
            (UnaryOpKind::Not, Expr(e, Some(Type::Bool))) => {
                UnaryOp(op, e.typed(Type::Bool).bx()).typed(Type::Bool)
            }
            (UnaryOpKind::Neg, Expr(e, Some(Type::Int))) => {
                UnaryOp(op, e.typed(Type::Int).bx()).typed(Type::Int)
            }
            x @ _ => panic!("unsupported {:?}", x),
        },
        BinaryOp(op, e1, e2) => match (op, typed_expr_ctx(ctx, *e1), typed_expr_ctx(ctx, *e2)) {
            (BinaryOpKind::Add, Expr(e1, Some(Type::Int)), Expr(e2, Some(Type::Int))) => {
                BinaryOp(op, e1.typed(Type::Int).bx(), e2.typed(Type::Int).bx()).typed(Type::Int)
            }
            (
                BinaryOpKind::And | BinaryOpKind::Or,
                Expr(e1, Some(Type::Bool)),
                Expr(e2, Some(Type::Bool)),
            ) => {
                BinaryOp(op, e1.typed(Type::Bool).bx(), e2.typed(Type::Bool).bx()).typed(Type::Bool)
            }
            (
                BinaryOpKind::CmpOp(CmpOpKind::Eq),
                Expr(e1, Some(Type::Bool)),
                Expr(e2, Some(Type::Bool)),
            ) => {
                BinaryOp(op, e1.typed(Type::Bool).bx(), e2.typed(Type::Bool).bx()).typed(Type::Bool)
            }
            (BinaryOpKind::CmpOp(_), Expr(e1, Some(Type::Int)), Expr(e2, Some(Type::Int))) => {
                BinaryOp(op, e1.typed(Type::Int).bx(), e2.typed(Type::Int).bx()).typed(Type::Bool)
            }
            x @ _ => panic!("type mismatch {:?}", x),
        },
        Tuple(es) => {
            let (es, ty) = es
                .into_iter()
                .map(|e| {
                    let Expr(e, ty) = typed_expr_ctx(ctx, e);
                    (Expr(e, ty.clone()), ty.unwrap())
                })
                .unzip();
            Tuple(es).typed(Type::Tuple(ty))
        }
        TupleRef(tu, idx) => {
            assert!(idx >= 0, "idx must be non-negative, got {}", idx);
            let Expr(tu, tuty) = typed_expr_ctx(ctx, *tu);
            let tuty = tuty.unwrap();
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
            TupleRef(tu.typed(tuty).bx(), idx).typed(elty)
        }
        TupleSet(tu, idx, val) => {
            assert!(idx >= 0, "idx must be non-negative, got {}", idx);
            let Expr(tu, tuty) = typed_expr_ctx(ctx, *tu);
            let Expr(val, valty) = typed_expr_ctx(ctx, *val);
            let (tuty, valty) = (tuty.unwrap(), valty.unwrap());

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
            TupleSet(tu.typed(tuty).bx(), idx, val.typed(valty).bx()).typed(Type::Void)
        }
        Collect(bytes) => Collect(bytes).typed(Type::Void),
        Allocate(1, ty @ Type::Tuple(..)) => Allocate(1, ty.clone()).typed(ty),
        x @ Allocate(..) => panic!("unimplemented {:?}", x),
        GlobalVar(x) => GlobalVar(x).typed(Type::Int),
        TupleLen(..) => unimplemented!(),
        Apply(_fun, _args) => unimplemented!(),
    }
}

pub fn typed_expr(e: Expr) -> Expr {
    typed_expr_ctx(&vec![], e)
}

fn typed_func(ctx: &Ctx, DefFun(name, params, retty, expr): DefFun) -> DefFun {
    let mut ctx = ctx.clone();
    for (name, ty) in &params {
        ctx = sym_set(&ctx, &name, &ty);
    }
    let expr = typed_expr_ctx(&ctx, expr);

    let Expr(_, ty) = &expr;
    assert!(!ty.is_none());

    DefFun(name, params, retty, expr)
}
pub fn typed_program(Program(funs, main): Program) -> Program {
    let gctx: Ctx = funs
        .iter()
        .cloned()
        .map(|DefFun(name, _, retty, _)| (name.clone(), retty.clone()))
        .collect();

    let funs: Vec<_> = funs.into_iter().map(|fun| typed_func(&gctx, fun)).collect();
    let main = typed_expr_ctx(&gctx, main);

    let Expr(_, ty) = &main;
    assert!(!ty.is_none());

    Program(funs, main)
}

/*
fn untyped_expr(Expr(expr, ty): Expr) -> Expr {
    assert!(!ty.is_none());
    use ExprK::*;
    match expr {
        Int(i) => Int(i).expr(),
        Bool(b) => Bool(b).expr(),
        Void => Void.expr(),
        Var(x) => Var(x).expr(),
        Let(x, expr, body) => Let(x, expr.untyped().bx(), body.untyped().bx()).expr(),
        If(pred, then_, else_) => If(
            pred.untyped().bx(),
            then_.untyped().bx(),
            else_.untyped().bx(),
        )
        .expr(),
        Read => Read.expr(),
        UnaryOp(op, expr) => UnaryOp(op, expr.untyped().bx()).expr(),
        BinaryOp(op, e1, e2) => BinaryOp(op, e1.untyped().bx(), e2.untyped().bx()).expr(),
        Tuple(es) => Tuple(es.into_iter().map(|e| e.untyped()).collect()).expr(),
        TupleRef(tu, idx) => TupleRef(tu.untyped().bx(), idx).expr(),
        TupleSet(tu, idx, val) => TupleSet(tu.untyped().bx(), idx, val.untyped().bx()).expr(),
        TupleLen(..) => unimplemented!(),
        Collect(..) => unimplemented!(),
        Allocate(..) => unimplemented!(),
        GlobalVar(..) => unimplemented!(),
    }
}
*/
