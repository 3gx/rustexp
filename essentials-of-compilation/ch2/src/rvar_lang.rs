#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Int(i64),
    Read,
    Neg(Box<Term>),
    Add(Box<Term>, Box<Term>),
    Var(String),
    Let(String, Box<Term>, Box<Term>),
}

pub macro add {
    ($e1:ident, $e2:ident) => {
        Term::Add(
            box var!(stringify!($e1)), box var!(stringify!($id))
        )
    },
    ($e1:ident, $e2:expr) => {
        Term::Add(box var!($e1), box $e2.into_term())
    },
    ($e1:expr, $id:ident) => {
        Term::Add(box $e1.into_term(), box var!(stringify!($id)))
    },
    ($e1:expr, $e2:expr) => {
        Term::Add(box $e1.into_term(), box $e2.into_term())
    },
}

pub macro neg {
    ($id:ident) => {
        neg!(box var!($id))
    },
    ($e:expr) => {
        Term::Neg(box $e.into_term())
    },
}

pub macro read() {
    Term::Read
}

pub macro int($e:expr) {
    Term::Int($e)
}

pub macro var {
    ($id:ident) => {
        var!(stringify!($id))
    },
    ($id:expr) => {
        Term::Var($id.to_string())
    }
}

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

#[derive(Debug, Clone)]
pub struct Options;

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Options>, pub Term);

pub macro program($e:expr) {
    Program(vec![], $e.into_term())
}

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

pub macro env {
    () => {
        Vec::new()
    },
    ($elem:expr; $n:expr) => {
        vec::from_elem($elem, $n)
    },
    ($($x:expr),+ $(,)?) => {
        <[_]>::into_vec(box [$($x),+])
    },
}

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
    use Term::*;
    match e {
        Int(n) => n.clone(),
        Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            input.trim().parse().unwrap()
        }
        Neg(e) => -interp_exp(env, e),
        Add(e1, e2) => interp_exp(env, e1) + interp_exp(env, e2),
        Var(x) => env_get(env, &x).unwrap().clone(),
        Let(x, e, body) => {
            let new_env = env_set(env, x, interp_exp(env, e));
            interp_exp(&new_env, body)
        }
    }
}

pub fn interp_program(p: &Program) -> Value {
    match p {
        Program(_, e) => interp_exp(&vec![], e),
    }
}

pub fn uniquify(_env: &Env, expr: &Term) -> Term {
    expr.clone()
}

#[cfg(test)]
mod tests {
    #[test]
    fn txx() {
        use crate::rvar_lang::*;
        let v = var!("x");
        println!("v= {:?}", v);
    }
}
