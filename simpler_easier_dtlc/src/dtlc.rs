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
