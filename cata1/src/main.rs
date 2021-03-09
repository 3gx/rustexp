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

fn main() {
    use Expr::*;
    let expr = Mul(Add(Value(1).bx(), Value(2).bx()).bx(), Value(3).bx());
    println!("expr= {:?}", expr);
    let val = eval_expr(&expr);
    println!("val= {:?}", val);
    let val1 = not_really_catta(eval_expr, &expr);
    println!("val= {:?}", val1);
    assert_eq!(val, val1);
}
