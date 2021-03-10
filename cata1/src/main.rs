#![feature(generic_associated_types)]
#![allow(incomplete_features)]

pub type Int = i64;

#[derive(Debug, Clone)]
pub enum Expr {
    Value(Int),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}
impl Expr {
    pub fn bx(self) -> Box<Self> {
        Box::new(self)
    }
}

pub fn eval_expr(e: &Expr) -> Int {
    use Expr::*;
    match e {
        Value(i) => *i,
        Add(e1, e2) => eval_expr(e1) + eval_expr(e2),
        Mul(e1, e2) => eval_expr(e1) * eval_expr(e2),
    }
}

pub fn not_really_catta(eval: impl Fn(&Expr) -> Int, e: &Expr) -> Int {
    eval(e)
}

#[derive(Debug, Clone)]
pub enum ExprF<A> {
    ValueF(Int),
    AddF(Box<A>, Box<A>),
    MulF(Box<A>, Box<A>),
}

impl<A> ExprF<A> {
    pub fn bx(self) -> Box<Self> {
        Box::new(self)
    }
}

trait Functor {
    type Unwrapped;
    type Wrapped<B>: Functor;
    fn map<F: Fn(Self::Unwrapped) -> B, B>(self, f: F) -> Self::Wrapped<B>;
}

impl<A> Functor for ExprF<A> {
    type Unwrapped = A;
    type Wrapped<B> = ExprF<B>;

    fn map<F: Fn(A) -> B, B>(self, f: F) -> ExprF<B> {
        match self {
            ExprF::ValueF(i) => ExprF::ValueF(i),
            ExprF::AddF(e1, e2) => ExprF::AddF(Box::new(f(*e1)), Box::new(f(*e2))),
            ExprF::MulF(e1, e2) => ExprF::MulF(Box::new(f(*e1)), Box::new(f(*e2))),
        }
    }
}

use std::fmt::Debug;

trait TFix {
    type Fix<F: Clone + Debug>: Clone + Debug;
}

struct Fix<F: TFix + Clone + Debug>(Box<F::Fix<Fix<F>>>);
impl<F: TFix + Clone + Debug> Clone for Fix<F> {
    fn clone(&self) -> Self {
        Fix(self.0.clone())
    }
}
impl<F: TFix + Clone + Debug> Debug for Fix<F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Fix").field(&self.0).finish()
    }
}

impl<F: Clone + Debug> TFix for ExprF<F> {
    type Fix<A: Clone + Debug> = ExprF<A>;
}

pub fn eval_exprf(e: &ExprF<Int>) -> Int {
    use ExprF::*;
    match e {
        ValueF(v) => *v,
        AddF(e1, e2) => (**e1) + (**e2),
        MulF(e1, e2) => (**e1) * (**e2),
    }
}

fn eval_fixed_expr(e: &Fix<ExprF<Int>>) -> Int {
    use ExprF::*;
    match &*e.0 {
        ValueF(i) => *i,
        AddF(e1, e2) => eval_fixed_expr(&*e1) + eval_fixed_expr(&*e2),
        MulF(e1, e2) => eval_fixed_expr(&*e1) * eval_fixed_expr(&*e2),
    }
}

fn main() {
    use Expr::*;
    let expr = Mul(Add(Value(1).bx(), Value(2).bx()).bx(), Value(3).bx());
    println!("expr= {:?}", expr);
    let val = eval_expr(&expr);
    println!("val= {:?}", val);
    let val1 = not_really_catta(eval_expr, &expr);
    println!("val= {:?}", val1);
    assert_eq!(val, val1);

    let exprf = ValueF(42);
    let valf = eval_exprf(&exprf);
    println!("exprf= {:?}, valf= {:?}", exprf, valf);
    let exprf = AddF(Box::new(2), Box::new(3));
    let valf = eval_exprf(&exprf);
    println!("exprf= {:?}, valf= {:?}", exprf, valf);
    let exprf = MulF(Box::new(2), Box::new(3));
    let valf = eval_exprf(&exprf);
    println!("exprf= {:?}, valf= {:?}", exprf, valf);
    use ExprF::*;
    let exprf: ExprF<ExprF<ExprF<Int>>> =
        MulF(AddF(ValueF(1).bx(), ValueF(2).bx()).bx(), ValueF(3).bx());
    println!("exprf= {:?}", exprf);

    let ef1: Fix<ExprF<_>> = Fix(ValueF(1).bx());
    println!("ef1= {:?}", ef1);
    let vf1 = eval_fixed_expr(&ef1.clone());
    println!("valf= {:?}", vf1);

    let ef2 = Fix(AddF(Box::new(Fix(ValueF(1).bx())), Box::new(Fix(ValueF(2).bx()))).bx());
    println!("e2= {:?}", ef2);
    let vf2 = eval_fixed_expr(&ef2);
    println!("vf2= {:?}", vf2);
}
