type Int = i64;
type Bool = bool;

// ------------------------------------------------------------------------------------------------

pub mod input_grammar {
    use super::*;

    pub enum CmpOpKind {
        Eq,
        Lt,
        Le,
        Gt,
        Ge,
    }

    pub enum UnaryOpKind {
        Neg,
        Not,
    }

    pub enum BinaryOpKind {
        CmpOp(CmpOpKind),
        Add,
        And,
        Or,
    }

    // input grammar
    // -------------
    pub enum Expr {
        // atoms
        Int(Int),
        Bool(Bool),
        Var(String),

        // let expr
        Let(String, Box<Expr>, Box<Expr>),

        //control-flow
        If(Box<Expr>, Box<Expr>, Box<Expr>),

        Read,

        UnaryOp(UnaryOpKind, Box<Expr>),
        BinaryOp(BinaryOpKind, Box<Expr>, Box<Expr>),

        Tuple(Vec<Expr>),
        TupleLen(Box<Expr>),
        TupleRef(Box<Expr>, Int),
        TupleSet(Box<Expr>, Int, Box<Expr>),
    }

    pub enum Value {
        Int(Int),
        Bool(Bool),
        Tuple(Vec<Value>),
        Void,
    }
}

// ------------------------------------------------------------------------------------------------

pub mod reduced_grammar {
    pub use super::*;

    // #[derive_from(input_grammar)]
    pub enum CmpOpKind {}

    // #[derive_from(input_grammar)]
    pub enum UnaryOpKind {}

    // #[derive_from(input_grammar)]
    pub enum BinaryOpKind {
        // #[rm] CmpOp,
        // #[rm] And,
        // #[rm] Or,
        Lt,
        Eq,
    }

    // #[derive_from(input_grammar)]
    pub enum Expr {}

    // #[derive_from(input_grammar)]
    pub enum Value {}
}

// ------------------------------------------------------------------------------------------------

pub mod typed_grammar {
    pub use super::*;

    pub enum Type {
        Bool,
        Int,
        Void,
        Tuple(Vec<Type>),
    }

    // #[derive_from(reduced_grammar)
    pub enum UnaryOpKind {}

    // #[derive_from(reduced_grammar)
    pub enum BinaryOpKind {}

    pub struct Expr(pub ExprK, pub Type);
    // #[derive_from_with_new_name(reduced_grammar::Expr)]
    pub enum ExprK {}

    // #[derive_from(reduced_grammar)
    pub enum Value {}
}

// ------------------------------------------------------------------------------------------------

pub mod remove_tuple_grammar {
    pub use super::*;

    // #[derive_from(typed_grammar)]
    pub enum Type {}

    // #[derive_from(typed_grammar)]
    pub enum UnaryOpKind {}

    // #[derive_from(typed_grammar)]
    pub enum BinaryOpKind {}

    pub struct Expr(pub ExprK, pub Type);

    // #[derive_from(typed_grammar)]
    pub enum ExprK {
        // #[rm] Tuple,
        Collect(Int),
        Allocate(Int, Type),
        GlobalVar(String),
    }

    // #[derive_from(typed_grammar)]
    pub enum Value {}
}

// ------------------------------------------------------------------------------------------------

pub mod anf_grammar {
    pub use super::*;

    pub enum Atom {
        Int(Int),
        Bool(Bool),
        Var(String),
    }

    // #[derive_from(reduced_grammar)]
    pub enum UnaryOpKind {}

    // #[derive_from(reduced_grammar)]
    pub enum BinaryOpKind {}

    // #[derive_from(typed_grammar)]
    pub enum Type {}

    // #[derived_from(reduced_grammar)]
    pub enum Expr {
        Collect(Int),
        Allocate(Int, Type),
        GlobalVar(String),
    }

    // #[derived_from(reduced_grammar)]
    pub enum Value {}
}

// ------------------------------------------------------------------------------------------------

pub mod c_grammar {
    pub use super::*;

    // #[derive_from(anf_grammar)]
    pub enum Atom {}

    // #[derive_from(anf_grammar)]
    pub enum UnaryOpKind {}

    // #[derive_from(anf_grammar)]
    pub enum BinaryOpKind {}

    // #[derive_from(anf_grammar)]
    pub enum Type {}

    // #[derive_from(anf_grammar)]
    pub enum Expr {
        /*
        #[rm] UnaryOp,
        #[rm] BinaryOp,
        #[rm] TupleRef,
        #[rm] TupleSet,
        */
        UnaryOp(UnaryOpKind, Atom),
        BinaryOp(BinaryOpKind, Atom, Atom),
        TupleRef(Atom, Int),
        TupleSet(Atom, Int, Atom),
    }

    pub enum Stmt {
        AssignVar(String, Expr),
    }

    pub enum Tail {
        Return(Expr),
        Seq(Stmt, Box<Tail>),
        Goto(String),
        IfStmt(Expr, String, String),
    }
}

// ------------------------------------------------------------------------------------------------

pub mod x86var_grammar {
    pub use super::*;

    pub enum Value {
        Int(Int),
        EFlag(CndCode),
    }

    pub enum CndCode {
        Eq,
        Lt,
        Gt,
    }

    pub enum Arg {
        Imm(Int),
        Var(String),
        EFlag,
    }

    pub enum BinaryKind {
        Addq,
        Subq,
        Movq,
        Xorq,
        Cmpq,
        Movzbq,
    }

    pub enum UnaryKind {
        Negq,
        Pushq,
        Popq,
        Set(CndCode),
    }

    pub enum Inst {
        Unary(UnaryKind, Arg),
        Binary(BinaryKind, Arg, Arg),
        Callq(String, Int /*arity*/),
        Retq,
        Jmp(String),
        JmpIf(CndCode, String),
    }
}

pub mod x86varreg_grammar {
    pub use super::*;

    // #[derive_from(x86var_grammar)]
    pub enum Value {}

    #[allow(non_camel_case_types)]
    pub enum Reg {
        rsp,
        rbp,
        rax,
        rbx, // 0
        rcx, // 1
        rdx, // 2
        rsi, // 3
        rdi, // 4
        r8,  // 5
        r9,  // 6
        r10, // 7
        r11, // 8
        r12, // 9
        r13, // 10
        r14, // 11
        r15, // 12
    }
    #[allow(non_camel_case_types)]
    pub enum ByteReg {
        ah,
        al,
        bh,
        bl,
        ch,
        cl,
        dh,
        dl,
    }

    // #[derive_from(x86var_grammar)]
    pub enum CndCode {}

    // #[derive_from(x86var_grammar)]
    pub enum Arg {
        Reg(Reg),
        ByteReg(ByteReg),
        Deref(Reg, Int),
    }

    // #[derive_from(x86var_grammar)]
    pub enum BinaryKind {}

    // #[derive_from(x86var_grammar)]
    pub enum UnaryKind {}

    // #[derive_from(x86var_grammar)]
    pub enum Inst {}
}

pub mod x86reg_grammar {

    // #[derive_from(x86var_grammar)]
    pub enum Value {}

    // #[derive_from(x86varreg_grammar)]
    pub enum Reg {}

    // #[derive_from(x86varreg_grammar)]
    pub enum ByteReg {}

    // #[derive_from(x86varreg_grammar)]
    pub enum CndCode {}

    // #[derive_from(x86varreg_grammar)]
    pub enum Arg {
        // #[rm] Var
    }

    // #[derive_from(x86varreg_grammar)]
    pub enum BinaryKind {}

    // #[derive_from(x86varreg_grammar)]
    pub enum UnaryKind {}

    // #[derive_from(x86varreg_grammar)]
    pub enum Inst {}
}
