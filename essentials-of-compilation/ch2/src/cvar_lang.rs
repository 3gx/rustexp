type Int = i64;
type Label = String;
type Info = Vec<i64>;
type Value = Int;
pub type Env = Vec<(Label, Value)>;

pub fn env_get<'a>(env: &'a Env, key: &str) -> Option<&'a Value> {
    env.iter()
        .rev()
        .find_map(|x| if x.0 != key { None } else { Some(&x.1) })
}

pub fn env_set(env: &Env, key: &str, val: Value) -> Env {
    let mut env = env.clone();
    env.push((key.to_string(), val));
    env
}

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

pub trait IntoAtom {
    type Output;
    fn into_atom(&self) -> Self::Output;
}

#[derive(Debug, Clone)]
pub enum Atm {
    Int(Int),
    Var(String),
}
pub macro int($e:expr) {
    Atm::Int($e)
}
pub macro var {
    ($id:ident) => {
        var!(stringify!($id))
    },
    ($id:expr) => {
        Atm::Var($id.to_string())
    }
}
impl IntoAtom for i64 {
    type Output = Atm;
    fn into_atom(&self) -> Atm {
        int!(*self)
    }
}
impl IntoAtom for &str {
    type Output = Atm;
    fn into_atom(&self) -> Atm {
        var!(self)
    }
}

#[derive(Debug, Clone)]
pub enum Prim {
    Read,
    Neg(Atm),
    Add(Atm, Atm),
}
pub macro read() {
    Prim::Read
}

pub macro neg {
    ($id:ident) => {
        neg!(var!($id))
    },
    ($e:expr) => {
        Prim(Neg, $e.into_atom())
    },
}

pub macro add {
    ($i1:ident, $i2:ident) => {
        Prim::Add(var!(stringify!($i1)), var!($i2))
    },
    ($i1:ident, $a2:expr) => {
        Prim::Add(var!($i1), $a2.into_atom())
    },
    ($a1:expr, $i2:ident) => {
        Prim::Add($a1.into_atom(), var!($i2)),
    },
    ($a1:expr, $a2:expr) => {
        Prim::Add($a1.into_atom(), $a2.into_atom())
    },
}

#[derive(Debug, Clone)]
pub enum Exp {
    Atm(Atm),
    Prim(Prim),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    AssignVar(String, Exp),
}

#[derive(Debug, Clone)]
pub enum Tail {
    Return(Exp),
    Seq(Stmt, Box<Tail>),
}

#[derive(Debug, Clone)]
pub struct CProgram(Info, Vec<(Label, Tail)>);

pub fn interp_atom(env: &Env, atom: &Atm) -> Value {
    use Atm::*;
    match atom {
        Int(n) => n.clone(),
        Var(x) => env_get(env, x).unwrap().clone(),
    }
}

pub fn interp_prim(env: &Env, prim: &Prim) -> Value {
    use Prim::*;
    match prim {
        Read => {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            input.trim().parse().unwrap()
        }
        Neg(atom) => -interp_atom(env, atom),
        Add(a1, a2) => interp_atom(env, a1) + interp_atom(env, a2),
    }
}

pub fn interp_exp(env: &Env, e: &Exp) -> Value {
    use Exp::*;
    match e {
        Atm(atom) => interp_atom(env, atom),
        Prim(prim) => interp_prim(env, prim),
    }
}

pub fn interp_stmt(env: &Env, stmt: &Stmt) -> Env {
    match stmt {
        Stmt::AssignVar(var, exp) => env_set(env, var, interp_exp(env, exp)),
    }
}

pub fn interp_tail(env: &Env, tail: &Tail) -> Value {
    match tail {
        Tail::Return(exp) => interp_exp(env, exp),
        Tail::Seq(stmt, tail) => {
            let new_env = interp_stmt(env, stmt);
            interp_tail(&new_env, tail)
        }
    }
}

#[path = "macros.rs"]
mod macros;

pub fn inter_prog(prog: &CProgram) -> Value {
    let CProgram(_, blocks) = prog;
    macros::r#match! { [ &blocks[..] ]
        [(s,tail)] if @{let "start" = &s[..]} =>  interp_tail(&env![], tail),
        _ => panic!("unhandled {:?}", blocks)
    }

    /*
    match prog {
        case!(CProgram(_, Vec[(String("start"), tail)]) => interp_tail(&env![], tail)),
        _ => panic!("unhandled {:?}", blocks)
    }

    becomes

    match prog {
        CProgram(_, vec) => match &vec[..] {
            &[str, tail] => match &str[..] {
                "start" => yay!
            },
        },
        _ => panic!("unhandled {:?}", blocks)
    }
    */
}
