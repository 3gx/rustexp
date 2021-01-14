#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Plus,
    Neg,
    Read,
}

#[derive(Debug, Clone)]
pub enum Term {
    Int(i64),
    Prim(OpCode, Vec<Term>),
    Var(String),
    Let(String, Box<Term>, Box<Term>),
}
#[macro_export]
macro_rules! rvar_plus {
    ($e1:ident, $e2:ident) => {
        Term::Prim(
            OpCode::Plus,
            vec![var!(stringify!($e1)), var!(stringify!($id))],
        )
    };
    ($e1:ident, $e2:expr) => {
        Term::Prim(OpCode::Plus, vec![var!($e1), $e2.into_term()])
    };
    ($e1:expr, $id:ident) => {
        Term::Prim(OpCode::Plus, vec![$e1.into_term(), var!(stringify!($id))])
    };
    ($e1:expr, $e2:expr) => {
        Term::Prim(OpCode::Plus, vec![$e1.into_term(), $e2.into_term()])
    };
}
pub use rvar_plus as plus;

#[macro_export]
macro_rules! rvar_neg {
    ($id:ident) => {
        neg!(var!($id))
    };
    ($e:expr) => {
        Term::Prim(OpCode::Neg, vec![$e.into_term()])
    };
}
pub use rvar_neg as neg;

#[macro_export]
macro_rules! rvar_read {
    () => {
        Term::Prim(OpCode::Read, vec![])
    };
}
pub use rvar_read as read;

#[macro_export]
macro_rules! rvar_int {
    ($e:expr) => {
        Term::Int($e)
    };
}
pub use rvar_int as int;

/*
#[macro_export]
macro_rules! rvar_var {
    ($id:ident) => {
        var!(stringify!($id))
    };
    ($id:expr) => {
        Term::Var($id.to_string())
    };
}
pub use rvar_var as var;
*/
pub macro var {
    ($id:ident) => {
        var!(stringify!($id))
    },
    ($id:expr) => {
        Term::Var($id.to_string())
    }
}

#[macro_export]
macro_rules! rvar_let {
    ([$id:ident <- $e1:expr]  $e2:ident) => {
        Term::Let(
            stringify!($id).to_owned(),
            Box::new($e1.into_term()),
            Box::new(var!(stringify!($e2))),
        )
    };
    ([$id:ident <- $e1:expr]  $e2:expr) => {
        Term::Let(
            stringify!($id).to_owned(),
            Box::new($e1.into_term()),
            Box::new($e2.into_term()),
        )
    };
}
pub use rvar_let as r#let;

#[derive(Debug, Clone)]
pub struct Options;

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Options>, pub Term);

#[macro_export]
macro_rules! rvar_program {
    ($e:expr) => {
        Program(vec![], $e.into_term())
    };
}
pub use rvar_program as program;

pub trait IntoTerm {
    fn into_term(&self) -> Term;
}

impl IntoTerm for i64 {
    fn into_term(&self) -> Term {
        int!(*self)
    }
}
impl IntoTerm for Term {
    fn into_term(&self) -> Term {
        self.clone()
    }
}
impl IntoTerm for &str {
    fn into_term(&self) -> Term {
        var!(self)
    }
}

type Value = i64;
pub type Env = Vec<(String, Value)>;

pub fn env_get<'a>(env: &'a Env, key: &str) -> Option<&'a Value> {
    //println!("env= {:?}", env);
    env.iter()
        .rev()
        .find_map(|x| if x.0 != key { None } else { Some(&x.1) })
}

pub fn env_set(env: &Env, key: &str, val: Value) -> Env {
    let mut env = env.clone();
    env.push((key.to_string(), val));
    env
}

pub fn interp_exp(env: &Env, e: &Term) -> Value {
    use {OpCode::*, Term::*};
    match e {
            Int(n) => n.clone(),
            Prim(Read, v) if let [] = &v[..] => {
                    let mut input = String::new();
                    std::io::stdin().read_line(&mut input).unwrap();
                    input.trim().parse().unwrap()
            },
            Prim(Neg, v) if let [e] = &v[..] => -interp_exp(env, e),
            Prim(Plus, v) if let [e1,e2] = &v[..] => interp_exp(env, e1) + interp_exp(env,e2),
            Var(x) => env_get(env, &x).unwrap().clone(),
            Let(x, e, body) => {
                let new_env = env_set(env,x, interp_exp(env,e));
                interp_exp(&new_env, body)
            }
            _ => panic!("unhandled term {:?}", e),
        }
}

pub fn interp_program(p: &Program) -> Value {
    match p {
        Program(_, e) => interp_exp(&vec![], e),
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn t1() {
        use crate::rvar_lang::*;
        let v = var!("x");
        println!("v= {:?}", v);
    }
}
