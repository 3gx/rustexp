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
trait VecOps {
    fn remove(self, el: Self) -> Self;
}

impl<T: Ord> VecOps for Vec<T> {
    fn remove(self, el: Self) -> Self {
        let el: BTreeSet<T> = el.into_iter().collect();
        self.into_iter().filter(|x| !el.get(x).is_none()).collect()
    }
}

fn free_vars(expr: Expr) -> Vec<Sym> {
    use ExprK::*;
    match expr.unbox() {
        Var(s) => vec![s],
        App(f, a) => [free_vars(f), free_vars(a)].concat(),
        Lam(i, t, e) => [free_vars(t), free_vars(e).remove(vec![i])].concat(),
        Pi(i, k, t) => [free_vars(k), free_vars(t).remove(vec![i])].concat(),
        Kind(_) => vec![],
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test1() {
        println!("hello world!");
    }
}
