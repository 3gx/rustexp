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
    pub fn deref(&self) -> &ExprK {
        &self.0
    }
}

impl From<ExprK> for Expr {
    fn from(item: ExprK) -> Self {
        Expr::new(item)
    }
}

use std::collections::BTreeSet;
trait SetOps {
    fn remove(&self, el: &Self) -> Self;
    fn union(&self, el: &Self) -> Self;
}

impl<T: Clone + Ord> SetOps for Vec<T> {
    fn remove(&self, y: &Self) -> Self {
        let y: BTreeSet<T> = y.iter().cloned().collect();
        self.iter()
            .filter(|x| y.get(x).is_none())
            .cloned()
            .collect()
    }
    fn union(&self, y: &Self) -> Self {
        let x: BTreeSet<&T> = self.iter().collect();
        let y = y.iter().filter(|z| x.get(z).is_none()).cloned();
        let mut x = self.clone();
        for el in y {
            x.push(el);
        }
        x
    }
}

fn free_vars(expr: &Expr) -> Vec<Sym> {
    use ExprK::*;
    match expr.deref() {
        Var(s) => vec![s.clone()],
        App(f, a) => free_vars(&f).union(&free_vars(&a)),
        Lam(i, t, e) => free_vars(&t).union(&free_vars(&e).remove(&vec![i.clone()])),
        Pi(i, k, t) => free_vars(&k).union(&free_vars(&t).remove(&vec![i.clone()])),
        Kind(_) => vec![],
    }
}

fn subst_var(s: &Sym, s1: &Sym, e: &Expr) -> Expr {
    subst(s, &ExprK::Var(s1.clone()).into(), e)
}
fn subst(v: &Sym, x: &Expr, e: &Expr) -> Expr {
    use ExprK::*;
    fn clone_sym(e: &Expr, i: &Sym) -> Sym {
        todo!()
    }
    let abstr = |i: &Sym, t: &Expr, e: &Expr, f: &dyn Fn(Sym, Expr, Expr) -> ExprK| {
        let fvx: BTreeSet<Sym> = free_vars(&x).iter().cloned().collect();
        if v == i {
            f(i.clone(), subst(v, x, t), e.clone())
        } else if !fvx.get(i).is_none() {
            let i1 = clone_sym(&e, &i);
            let e1 = subst_var(&i, &i1, e);
            f(i1, subst(v, x, t), subst(v, x, &e1))
        } else {
            f(i.clone(), subst(v, x, t), subst(v, x, e))
        }
    };
    match e.deref() {
        e @ Var(i) => {
            if v == i {
                x.clone()
            } else {
                e.clone().into()
            }
        }
        App(f, a) => App(subst(v, x, f), subst(v, x, a)).into(),
        Lam(i, t, e) => abstr(i, t, e, &|x, y, z| Lam(x, y, z)).into(),
        Pi(i, t, e) => abstr(i, t, e, &|x, y, z| Pi(x, y, z)).into(),
        Kind(k) => Kind(k.clone()).into(),
    }
}

fn fix1<T, R, F: Fn(&dyn Fn(T) -> R, T) -> R>(f: F) -> impl Fn(T) -> R {
    fn fix_impl<T, R, F: Fn(&dyn Fn(T) -> R, T) -> R>(f: &F, t: T) -> R {
        f(&|t| fix_impl(f, t), t)
    }
    move |t| fix_impl(&f, t)
}
fn fix2<T1, T2, R, F: Fn(&dyn Fn(T1, T2) -> R, T1, T2) -> R>(f: F) -> impl Fn(T1, T2) -> R {
    fn fix_impl<T1, T2, R, F: Fn(&dyn Fn(T1, T2) -> R, T1, T2) -> R>(f: &F, t1: T1, t2: T2) -> R {
        f(&|t1, t2| fix_impl(f, t1, t2), t1, t2)
    }
    move |t1, t2| fix_impl(&f, t1, t2)
}

fn nf(ee: &Expr) -> Expr {
    let spine = fix2(|spine, e: Expr, r#as: Vec<Expr>| {
        use ExprK::*;
        let app = |f, r#as: Vec<Expr>| {
            r#as.iter()
                .fold(Expr::new(f), |acc, x| Expr::new(ExprK::App(acc, x.clone())))
        };
        match (e.deref(), &r#as[..]) {
            (App(f, e), [r#as @ ..]) => spine(f.clone(), [&[e.clone()], &r#as[..]].concat()),
            (Lam(s, t, e), []) => Lam(s.clone(), nf(t), nf(e)).into(),
            (Lam(s, _, e), [a, r#as @ ..]) => spine(subst(&s, a, e), r#as.to_vec()),
            (Pi(s, k, t), [r#as @ ..]) => app(Pi(s.clone(), nf(k), nf(t)), r#as.to_vec()),
            (f, r#as) => app(f.clone(), r#as.to_vec()),
        }
    });
    /*
    fn spine(e: &Expr, r#as: Vec<Expr>) -> Expr {
        use ExprK::*;
        let app = |f, r#as: Vec<Expr>| {
            r#as.iter()
                .fold(Expr::new(f), |acc, x| Expr::new(ExprK::App(acc, x.clone())))
        };
        match (e.deref(), &r#as[..]) {
            (App(f, e), [r#as @ ..]) => spine(f, [&[e.clone()], &r#as[..]].concat()),
            (Lam(s, t, e), []) => Lam(s.clone(), nf(t), nf(e)).into(),
            (Lam(s, _, e), [a, r#as @ ..]) => spine(&subst(&s, a, e), r#as.to_vec()),
            (Pi(s, k, t), [r#as @ ..]) => app(Pi(s.clone(), nf(k), nf(t)), r#as.to_vec()),
            (f, r#as) => app(f.clone(), r#as.to_vec()),
        }
    }
        */
    spine(ee.clone(), vec![])
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test1() {
        let x: Vec<Sym> = vec!["x".into(), "y".into(), "z".into()];
        let y: Vec<Sym> = vec!["u".into(), "v".into()];
        let z = x.union(&y);
        let r: Vec<Sym> = vec!["x".into(), "y".into(), "z".into(), "u".into(), "v".into()];
        assert_eq!(z, r);

        let x: Vec<Sym> = vec!["x".into(), "u".into(), "z".into()];
        let y: Vec<Sym> = vec!["u".into(), "v".into()];
        let z = x.union(&y);
        let r: Vec<Sym> = vec!["x".into(), "u".into(), "z".into(), "v".into()];
        assert_eq!(z, r);

        let x: Vec<Sym> = vec!["x".into(), "u".into(), "z".into()];
        let y: Vec<Sym> = vec!["u".into(), "v".into()];
        let z = x.remove(&y);
        let r: Vec<Sym> = vec!["x".into(), "z".into()];
        assert_eq!(z, r);

        let x: Vec<Sym> = vec![];
        let y: Vec<Sym> = vec!["u".into(), "v".into()];
        let z = x.union(&y);
        let r: Vec<Sym> = vec!["u".into(), "v".into()];
        assert_eq!(z, r);
    }

    #[test]
    fn test2() {
        #[derive(Clone)]
        struct Int(i32);
        let val = Int(1);
        let f = {
            let val = val.clone();
            fix1(move |f, n| if n == 1 { val.0 } else { n * f(n - 1) })
        };
        let v: i32 = f(5);
        println!("{}", v);
        let f1 = fix1(|f, n| if n == 1 { val.0 } else { n * f(n - 1) });
        let v1: i32 = f1(5);
        println!("{}", v1);
        let f2 = fix2(|f, n, m| if n == 1 { m } else { n * f(n - 1, m) });
        let v2: i32 = f2(5, 1);
        println!("{}", v2);
    }
}
