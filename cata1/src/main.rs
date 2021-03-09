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

fn main() {
    use Expr::*;
    let expr = Mul(Add(Value(1).bx(), Value(2).bx()).bx(), Value(3).bx());
    println!("expr= {:?}", expr);
    let val = eval_expr(&expr);
    println!("val= {:?}", val);
    let val1 = not_really_catta(eval_expr, &expr);
    println!("val= {:?}", val1);
    assert_eq!(val, val1);

    use ExprF::*;
    let exprf: ExprF<ExprF<ExprF<Int>>> =
        MulF(AddF(ValueF(1).bx(), ValueF(2).bx()).bx(), ValueF(3).bx());
    println!("exprf= {:?}", exprf);
}
