#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(dead_code)]

type Sym = String;
type Type = Expr;

#[derive(Debug, Clone)]
pub enum ExprK {
    Var(Sym),
    App(Expr, Expr),
    Lam(Sym, Type, Expr),
    Pi(Sym, Type, Type),
    Kind(Kinds),
}

#[derive(Debug, Clone)]
pub enum Kinds {
    Star,
    Box,
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

use std::collections::BTreeSet;
trait SetOps {
    fn remove(self, el: Self) -> Self;
    fn union(self, el: Self) -> Self;
}

impl<T: Ord> SetOps for Vec<T> {
    fn remove(self, y: Self) -> Self {
        let y: BTreeSet<T> = y.into_iter().collect();
        self.into_iter().filter(|x| y.get(x).is_none()).collect()
    }
    fn union(mut self, y: Self) -> Self {
        let x: BTreeSet<&T> = self.iter().collect();
        let y: Self = y.into_iter().filter(|z| x.get(z).is_none()).collect();
        for el in y.into_iter() {
            self.push(el)
        }
        self
    }
}

fn free_vars(expr: Expr) -> Vec<Sym> {
    use ExprK::*;
    match expr.unbox() {
        Var(s) => vec![s],
        App(f, a) => free_vars(f).union(free_vars(a)),
        Lam(i, t, e) => free_vars(t).union(free_vars(e).remove(vec![i])),
        Pi(i, k, t) => free_vars(k).union(free_vars(t).remove(vec![i])),
        Kind(_) => vec![],
    }
}

fn subst_var(s: &Sym, s1: Sym, e: Expr) -> Expr {
    subst(s, ExprK::Var(s1).into(), e)
}
fn subst(v: &Sym, x: Expr, e: Expr) -> Expr {
    use ExprK::*;
    fn clone_sym(e: &Expr, i: &Sym) -> Sym {
        todo!()
    }
    fn abstr(
        v: &Sym,
        x: Expr,
        i: Sym,
        t: Expr,
        e: Expr,
        f: impl Fn(Sym, Expr, Expr) -> ExprK,
    ) -> ExprK {
        let fvx: BTreeSet<Sym> = free_vars(x.clone()).into_iter().collect();
        if v == &i {
            f(i, subst(v, x, t), e)
        } else if !fvx.get(&i).is_none() {
            let i1 = clone_sym(&e, &i);
            let e1 = subst_var(&i, i1.clone(), e);
            f(i1, subst(v, x.clone(), t), subst(v, x, e1))
        } else {
            f(i, subst(v, x.clone(), t), subst(v, x.clone(), e))
        }
    }
    match e.unbox() {
        Var(i) => {
            if v == &i {
                x
            } else {
                Var(i).into()
            }
        }
        App(f, a) => App(subst(v, x.clone(), f), subst(v, x, a)).into(),
        Lam(i, t, e) => abstr(v, x, i, t, e, |x, y, z| Lam(x, y, z)).into(),
        Pi(i, t, e) => abstr(v, x, i, t, e, |x, y, z| Pi(x, y, z)).into(),
        Kind(k) => Kind(k).into(),
    }
}

fn nf(ee: Expr) -> Expr {
    fn spine(e: Expr, r#as: Vec<Expr>) -> Expr {
        use ExprK::*;
        fn app(e: Expr, r#as: Vec<Expr>) -> Expr {
            todo!()
        }
        match (e.unbox(), &r#as[..]) {
            (App(f, e), [r#as @ ..]) => spine(f, [&[e], &r#as[..]].concat()),
            (Lam(s, t, e), []) => Lam(s, nf(t), nf(e)).into(),
            (Lam(s, _, e), [a, r#as @ ..]) => spine(subst(&s, a.clone(), e), r#as.to_vec()),
            (Pi(s, k, t), [r#as @ ..]) => app(Pi(s, nf(k), nf(t)).into(), r#as.to_vec()),
            (f, r#as) => app(f.into(), r#as.to_vec()),
        }
    }
    spine(ee, vec![])
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test1() {
        let x: Vec<Sym> = vec!["x".into(), "y".into(), "z".into()];
        let y: Vec<Sym> = vec!["u".into(), "v".into()];
        let z = x.union(y);
        let r: Vec<Sym> = vec!["x".into(), "y".into(), "z".into(), "u".into(), "v".into()];
        assert_eq!(z, r);

        let x: Vec<Sym> = vec!["x".into(), "u".into(), "z".into()];
        let y: Vec<Sym> = vec!["u".into(), "v".into()];
        let z = x.union(y);
        let r: Vec<Sym> = vec!["x".into(), "u".into(), "z".into(), "v".into()];
        assert_eq!(z, r);

        let x: Vec<Sym> = vec!["x".into(), "u".into(), "z".into()];
        let y: Vec<Sym> = vec!["u".into(), "v".into()];
        let z = x.remove(y);
        let r: Vec<Sym> = vec!["x".into(), "z".into()];
        assert_eq!(z, r);

        let x: Vec<Sym> = vec![];
        let y: Vec<Sym> = vec!["u".into(), "v".into()];
        let z = x.union(y);
        let r: Vec<Sym> = vec!["u".into(), "v".into()];
        assert_eq!(z, r);
    }
}
