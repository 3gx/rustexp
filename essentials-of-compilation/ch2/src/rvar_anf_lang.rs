type Int = i64;
type Label = String;
type Info = Vec<i64>;
type Value = Int;
pub type Env = Vec<(Label, Value)>;

#[derive(Debug, Clone)]
pub enum Atom {
    Int(Int),
    Var(String),
}
pub macro int($e:expr) {
    Atom::Int($e)
}
pub macro var {
    ($id:ident) => {
        var!(stringify!($id))
    },
    ($id:expr) => {
        Atom::Var($id.to_string())
    }
}
pub trait IntoAtom {
    type Output;
    fn into_atom(&self) -> Self::Output;
}

impl IntoAtom for i64 {
    type Output = Atom;
    fn into_atom(&self) -> Atom {
        int!(*self)
    }
}
impl IntoAtom for &str {
    type Output = Atom;
    fn into_atom(&self) -> Atom {
        var!(self)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(Atom),
    Read,
    Neg(Atom),
    Add(Atom, Atom),
    Let(String, Box<Expr>, Box<Expr>),
}

pub macro read() {
    Expr::Read
}

pub macro neg {
    ($id:ident) => {
        neg!(var!($id))
    },
    ($e:expr) => {
        Expr(Neg, $e.into_atom())
    },
}

pub macro add {
    ($i1:ident, $i2:ident) => {
        Expr::Add(var!(stringify!($i1)), var!($i2))
    },
    ($i1:ident, $a2:expr) => {
        Expr::Add(var!($i1), $a2.into_atom())
    },
    ($a1:expr, $i2:ident) => {
        Expr::Add($a1.into_atom(), var!($i2)),
    },
    ($a1:expr, $a2:expr) => {
        Expr::Add($a1.into_atom(), $a2.into_atom())
    },
}

pub fn remove_complex_operas() {
    unimplemented!()
}

#[path = "cvar_lang.rs"]
mod cvar_lang;
#[path = "./macros.rs"]
mod macros;
#[path = "rvar_lang.rs"]
mod rvar_lang;

#[derive(Debug, Clone)]
pub struct Program(Info, Expr);

pub macro r#let {
    ([$id:ident $e1:expr]  $e2:ident) => {
        Term::Let(
            stringify!($id).to_owned(),
            Box::new($e1.into_term()),
            Box::new(var!(stringify!($e2))),
        )
    },
    ([$id:ident $e1:expr]  $e2:expr) => {
        Term::Let(
            stringify!($id).to_owned(),
            Box::new($e1.into_term()),
            Box::new($e2.into_term()),
        )
    }
}

#[cfg(test)]
mod rvar_anf_lang {
    #[test]
    fn t1() {
        let p1 = {
            use super::rvar_lang::*;
            r#let!([x add!(12, 20)]  add!(10, x))
        };
        println!("p1= {:?} ", p1);
    }
    #[test]
    fn cvar1() {
        use super::cvar_lang;
        use super::cvar_lang::*;
        let v = int!(42);
        println!("v= {:?}", v);
        let v = var!(x);
        println!("v= {:?}", v);

        let exp = Exp::Prim(add!(10, 32));
        println!("expr= {:?}", exp);
        println!("res= {}", interp_exp(&cvar_lang::env![], &exp));
    }

    #[test]
    fn ast1() {
        use super::macros::r#match;
        #[derive(Debug, Clone)]
        enum OpCode {
            Neg,
            Add,
        }
        #[derive(Debug, Clone)]
        enum Expr {
            Int(i64),
            Prim(OpCode, Vec<Expr>),
        }
        use Expr::*;
        use OpCode::*;

        let e0 = Int(32);
        let e1 = Prim(Neg, vec![Int(32)]);
        let e1a = Prim(Neg, vec![Prim(Add, vec![Int(10), Int(32)])]);
        let e2 = Prim(
            Add,
            vec![Prim(Add, vec![Int(32), Prim(Neg, vec![Int(44)])]), Int(42)],
        );
        println!("e0= {:?}", e0);
        println!("e1= {:?}", e1);
        println!("e1a= {:?}", e1a);
        println!("e2= {:?}", e2);

        fn eval(e: &Expr) -> i64 {
            r#match! { [e]
                Int(n) => *n,
                Prim(Neg, v) if @{let [Int(m)] = v.as_slice()} => -*m,
                Prim(Neg, v) if @{let [e] = &v[..]} => -eval(e),
                Prim(Add, v) if @{let [e1,e2] = &v[..]} => eval(e1) + eval(e2),
                _ => panic!("unhandled {:?}", e)
            }
        }
        println!("eval(e0)= {}", eval(&e0));
        println!("eval(e1)= {}", eval(&e1));
        println!("eval(e1a)= {}", eval(&e1a));
        println!("eval(e2)= {}", eval(&e2));
    }
}
