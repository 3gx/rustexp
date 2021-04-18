type Sym = String;

#[derive(Debug, Clone)]
pub enum ExprK {
    Var(Sym),
    App(Expr, Expr),
    Lam(Sym, Expr),
}

#[derive(Debug, Clone)]
pub struct Expr(pub Box<ExprK>);
impl Expr {
    pub fn new(item: ExprK) -> Self {
        Expr(Box::new(item))
    }
}

impl From<ExprK> for Expr {
    fn from(item: ExprK) -> Self {
        Expr::new(item)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test1() {
        use ExprK::*;
        let expr: Expr = App(
            Lam("x".into(), Lam("y".into(), Var("y".into()).into()).into()).into(),
            Lam("z".into(), Var("z".into()).into()).into(),
        )
        .into();
        let fmt = format!("{:?}", expr);
        assert_eq!(fmt, "Expr(App(Expr(Lam(\"x\", Expr(Lam(\"y\", Expr(Var(\"y\")))))), Expr(Lam(\"z\", Expr(Var(\"z\"))))))");
        println!("expr= {}", fmt);
    }
}
