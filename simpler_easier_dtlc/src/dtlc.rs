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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kinds {
    Star,
    Box,
}

#[derive(Clone)]
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

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.deref() {
            ExprK::Kind(Kinds::Star) => f.write_fmt(format_args!("*")),
            ExprK::Kind(Kinds::Box) => f.write_fmt(format_args!("[]")),
            ExprK::Var(sym) => f.write_fmt(format_args!("{:?}", sym)),
            x @ _ => x.fmt(f),
        }
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
    /*
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
    spine(ee.clone(), vec![])
    */
    fn spine(e: &Expr, r#as: Vec<Expr>) -> Expr {
        use ExprK::*;
        let app = |f, r#as: Vec<Expr>| {
            r#as.iter()
                .map(|x| nf(x))
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
    spine(ee, vec![])
}

/*
fn whnf(ee: &Expr) -> Expr {
    todo!();
    fn spine(e: &Expr, r#as: Vec<Expr>) -> Expr {
        use ExprK::*;
        let app = |f, r#as: Vec<Expr>| {
            r#as.iter()
                .fold(Expr::new(f), |acc, x| Expr::new(ExprK::App(acc, x.clone())))
        };
        match (e.deref(), &r#as[..]) {
            (App(f, e), [r#as @ ..]) => spine(f, [&[e.clone()], &r#as[..]].concat()),
            (Lam(s, t, e), []) => Lam(s.clone(), whnf(t), whnf(e)).into(),
            (Lam(s, _, e), [a, r#as @ ..]) => spine(&subst(&s, a, e), r#as.to_vec()),
            (Pi(s, k, t), [r#as @ ..]) => app(Pi(s.clone(), whnf(k), whnf(t)), r#as.to_vec()),
            (f, r#as) => app(f.clone(), r#as.to_vec()),
        }
    }
    spine(ee, vec![])
}
*/

fn alpha_eq(e1: &Expr, e2: &Expr) -> bool {
    use ExprK::*;
    match (e1.deref(), e2.deref()) {
        (Var(v1), Var(v2)) => v1 == v2,
        (App(f1, a1), App(f2, a2)) => alpha_eq(f1, f2) && alpha_eq(a1, a2),
        (Lam(s1, t1, e1), Lam(s2, t2, e2)) => {
            alpha_eq(e1, &subst_var(s2, s1, e2)) && alpha_eq(t1, &subst_var(s2, s1, t2))
        }
        (Pi(s1, t1, e1), Pi(s2, t2, e2)) => {
            alpha_eq(e1, &subst_var(s2, s1, e2)) && alpha_eq(t1, &subst_var(s2, s1, t2))
        }
        (Kind(k1), Kind(k2)) => k1 == k2,
        _ => false,
    }
}
fn beta_eq(e1: &Expr, e2: &Expr) -> bool {
    alpha_eq(&nf(e1), &nf(e2))
}

#[derive(Debug, Clone)]
pub struct Env(pub Vec<(Sym, Type)>);
impl Env {
    fn new() -> Self {
        Env(vec![])
    }
}

type SafeType = Result<Type, String>;

fn find_var(r: &Env, s: &Sym) -> SafeType {
    for (sym, typ) in r.0.iter() {
        if sym == s {
            return Ok(typ.clone());
        }
    }
    Err(format!("symbol {:?} not found", s))
}

fn tcheck_red(r: &Env, e: &Expr) -> SafeType {
    Ok(nf(&tcheck(r, e)?))
}

fn extend(s: &Sym, t: &Type, Env(r): &Env) -> Env {
    Env([&[(s.clone(), t.clone())], &r[..]].concat())
}

fn valid_kind(s: &Type, t: &Type) -> bool {
    use ExprK::Kind;
    use Kinds::*;
    match (s.deref(), t.deref()) {
        (Kind(Star), Kind(Star)) => true,
        (Kind(Box), Kind(Star)) => true,
        (Kind(Star), Kind(Box)) => true,
        (Kind(Box), Kind(Box)) => true,
        _ => false,
    }
}

fn tcheck(r: &Env, e: &Expr) -> SafeType {
    use ExprK::*;
    match e.deref() {
        Var(s) => find_var(r, s),
        App(f, a) => {
            let tf = tcheck_red(r, f)?;
            match tf.unbox() {
                Pi(x, at, rt) => {
                    let ta = tcheck_red(r, a)?;
                    match beta_eq(&ta, &at) {
                        false => Err(format!("bad func argument: {:?} != {:?}", ta, at)),
                        true => Ok(subst(&x, a, &rt)),
                    }
                }
                x @ _ => Err(format!("non-function application: {:?}", x)),
            }
        }
        Lam(s, t, e) => {
            tcheck(r, t)?;
            let r1 = extend(s, t, r);
            let te = tcheck(&r1, e)?;
            let lt = Pi(s.clone(), t.clone(), te).into();
            tcheck(r, &lt)?;
            Ok(lt)
        }
        Kind(Kinds::Star) => Ok(Kind(Kinds::Box).into()),
        Kind(Kinds::Box) => Err("found a box".into()),
        Pi(x, a, b) => {
            let s = tcheck_red(r, a)?;
            let r1 = extend(x, a, r);
            let t = tcheck_red(&r1, b)?;
            if !valid_kind(&s, &t) {
                Err(format!("bad abstraction: {:?}", (s, t)))
            } else {
                Ok(t)
            }
        }
    }
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

    #[test]
    fn test3() {
        use ExprK::*;
        use Kinds::*;
        let id: Expr = Lam(
            "a".into(),
            Kind(Star).into(),
            Lam("x".into(), Var("a".into()).into(), Var("x".into()).into()).into(),
        )
        .into();
        let id_str = format!("{:?}", id);
        println!("id= {}", id_str);
        assert_eq!(id_str, "Lam(\"a\", *, Lam(\"x\", \"a\", \"x\"))");
    }

    #[test]
    fn test4() {
        use ExprK::*;
        use Kinds::*;
        let var = |v: &str| -> Expr { Var(v.into()).into() };
        let lam = |v: &str, typ: Expr, e: Expr| -> Expr { Lam(v.into(), typ, e).into() };
        let app = |e1: Expr, e2: Expr| -> Expr { App(e1, e2).into() };
        let star = || -> Expr { Kind(Star).into() };
        let pi = |v: &str, t: Expr, e: Expr| -> Expr { Pi(v.into(), t, e).into() };
        let r#box = || -> Expr { Kind(Box).into() };

        let zero = lam("s", pi("_1", star(), star()), lam("z", star(), var("z")));
        let one = lam(
            "s",
            pi("_2", star(), star()),
            lam("z", star(), app(var("s"), var("z"))),
        );
        let two = lam(
            "s",
            pi("_2", star(), star()),
            lam("z", star(), app(var("s"), app(var("s"), var("z")))),
        );
        let three = lam(
            "s",
            pi("_2", star(), star()),
            lam(
                "z",
                star(),
                app(var("s"), app(var("s"), app(var("s"), var("z")))),
            ),
        );
        println!("zero= {:?}", zero);
        println!("one= {:?}", one);
        println!("two= {:?}", two);
        println!("three= {:?}", three);

        println!("type(zero) => {:?}", tcheck(&Env::new(), &zero));
        println!(
            "type(type(zero)) => {:?}",
            tcheck(&Env::new(), &tcheck(&Env::new(), &zero).ok().unwrap())
        );
        println!("type(one) => {:?}", tcheck(&Env::new(), &one));
        println!("type(two) => {:?}", tcheck(&Env::new(), &two));
        println!("type(three) => {:?}", tcheck(&Env::new(), &three));

        let app2 = |f: Expr, x: Expr, y: Expr| -> Expr { App(App(f, x).into(), y).into() };

        let plus = lam(
            "m",
            pi("_m1", pi("_m1", star(), star()), pi("_m2", star(), star())),
            lam(
                "n",
                pi("_n1", pi("_n1", star(), star()), pi("_n2", star(), star())),
                lam(
                    "s",
                    pi("_s", star(), star()),
                    lam(
                        "z",
                        star(),
                        app2(var("m"), var("s"), app2(var("n"), var("s"), var("z"))),
                    ),
                ),
            ),
        );
        println!("plus= {:?}", plus);
        println!("type(plus)= {:?}", tcheck(&Env::new(), &plus));

        let three_add = app2(plus, one, two);
        println!("three_add= {:?}", three_add);
        println!("type(three_add)= {:?}", tcheck(&Env::new(), &three_add));

        let three_nf = nf(&three_add);
        println!("three_nf= {:?}", three_nf);
        println!("three   = {:?}", three);

        assert!(beta_eq(&three, &three_add));
    }
}
