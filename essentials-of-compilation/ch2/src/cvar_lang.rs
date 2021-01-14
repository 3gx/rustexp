type Int = i64;
type Label = String;
type Var = String;
type Info = Vec<i64>;

pub enum Atm {
    Int(Int),
    Var(Var),
}

pub enum Prim {
    Read,
    Neg(Atm),
    Add(Atm, Atm),
}

pub enum Exp {
    Atm,
    Read,
    Prim,
}

pub enum Stmt {
    Assign(Var, Exp),
}

pub enum Tail {
    Return(Exp),
    Seq(Stmt, Box<Tail>),
}

pub enum CVar {
    CProgram(Info, Vec<(Label, Tail)>),
}
