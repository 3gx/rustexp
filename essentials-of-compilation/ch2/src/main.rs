#![allow(incomplete_features)]
#![feature(if_let_guard)]

pub mod x86int {
    #[allow(non_camel_case_types)]
    pub enum Reg {
        rsp,
        rbp,
        rax,
        rbx,
        rcx,
        rdx,
        rsi,
        rdi,
        r8,
        r9,
        r10,
        r11,
        r12,
        r13,
        r14,
        r15,
    }

    type Int = i64;
    pub enum Arg {
        Imm(Int),
        Reg(Reg),
        Deref(Reg, Int),
    }

    type Label = String;
    pub enum Instr {
        Addq(Arg, Arg),
        Subq(Arg, Arg),
        Movq(Arg, Arg),
        Negq(Arg, Arg),
        Callq(Label, Int),
        Retq,
        Pushq(Arg),
        Popq(Arg),
        Jmp(Label),
    }

    type Info = Vec<i64>;
    pub struct Block(Info, Vec<Instr>);
}

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
macro_rules! plus {
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
macro_rules! neg {
    ($id:ident) => {
        neg!(var!($id))
    };
    ($e:expr) => {
        Term::Prim(OpCode::Neg, vec![$e.into_term()])
    };
}
macro_rules! read {
    () => {
        Term::Prim(OpCode::Read, vec![])
    };
}
macro_rules! int {
    ($e:expr) => {
        Term::Int($e)
    };
}

macro_rules! var {
    ($id:ident) => {
        var!(stringify!($id))
    };
    ($id:expr) => {
        Term::Var($id.to_string())
    };
}
macro_rules! r#let {
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

#[derive(Debug, Clone)]
pub struct Options;

#[derive(Debug, Clone)]
pub struct Program(Vec<Options>, Term);
macro_rules! program {
    ($e:expr) => {
        Program(vec![], $e.into_term())
    };
}
trait IntoTerm {
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

fn main() {
    {
        let p1 = program![r#let!([x <- plus!(12, 20)]  plus!(10, x))];
        println!("p1= {:?} ", p1);
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = program![r#let!([x <- 2]  plus!(r#let!([x <- 10]  x), x))];
        println!("p1= {:?} ", p1);
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);

        let p1 = program![r#let!(
            [x <- read!()]
            r#let!([y <- read!()]  plus!(x, neg!(y)))
        )];
        println!("p1= {:?} ", p1);
        println!("inter 52<enter>, 10<enter>, should get 42");
        let v1 = interp_program(&p1);
        println!("v1= {:?} ", v1);
    }
}
