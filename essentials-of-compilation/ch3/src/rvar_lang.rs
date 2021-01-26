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
            Box::new(var!(stringify!($e1))), Box::new(var!(stringify!($id)))
        )
    },
    ($e1:ident, $e2:expr) => {
        Term::Add(Box::new(var!($e1)), Box::new($e2.into_term()))
    },
    ($e1:expr, $id:ident) => {
        Term::Add(Box::new($e1.into_term()), Box::new(var!(stringify!($id))))
    },
    ($e1:expr, $e2:expr) => {
        Term::Add(Box::new($e1.into_term()), Box::new($e2.into_term()))
    },
}

pub macro neg {
    ($id:ident) => {
        neg!(Box::new(var!($id)))
    },
    ($e:expr) => {
        Term::Neg(Box::new($e.into_term()))
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
