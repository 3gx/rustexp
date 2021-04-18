#![allow(unused_variables)]
#![allow(dead_code)]
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
    pub fn unbox(self) -> ExprK {
        *self.0
    }
}

impl From<ExprK> for Expr {
    fn from(item: ExprK) -> Self {
        Expr::new(item)
    }
}

fn subst(v: Sym, x: Expr, b: Expr) -> Expr {
    todo!()
}

fn whnf(ee: Expr) -> Expr {
    fn spine(ee: Expr, r#as: Vec<Expr>) -> Expr {
        match (ee.unbox(), &r#as[..]) {
            (ExprK::App(f, a), [rest @ ..]) => spine(f, [&[a], &rest[..]].concat()),
            (ExprK::Lam(s, e), [a, rest @ ..]) => spine(subst(s, a.clone(), e), rest.to_vec()),
            (f, r#as) => r#as
                .iter()
                .fold(Expr::new(f), |acc, x| Expr::new(ExprK::App(acc, x.clone()))),
        }

        /*
        match ee.unbox() {
            ExprK::App(f, a) => spine(f, [&[a], &r#as[..]].concat()),
            //            ExprK::Lam(s, e) => spine(subst(s, r#as[0], e), r#as[1..].to_vec()),
            _ => todo!(),
        }
        */
    }
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test1() {
        use ExprK::*;
        let expr: Expr = App(
            Lam("x".into(), Lam("y".into(), Var("x".into()).into()).into()).into(),
            Lam("z".into(), Var("z".into()).into()).into(),
        )
        .into();
        let fmt = format!("{:?}", expr);
        assert_eq!(fmt, "Expr(App(Expr(Lam(\"x\", Expr(Lam(\"y\", Expr(Var(\"x\")))))), Expr(Lam(\"z\", Expr(Var(\"z\"))))))");
        println!("expr= {}", fmt);
    }
}
