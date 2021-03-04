pub mod input_grammar {
    type Int = i64;
    type Bool = bool;

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
    }
}

pub mod reduce_grammar {
    type Int = i64;
    type Bool = bool;

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
        And,
        Lt,
        Eq,
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
    }
}

pub mod typed_grammar {
    type Int = i64;
    type Bool = bool;

    pub enum Type {
        Bool,
        Int,
        Void,
        Tuple(Vec<Type>),
    }

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
        And,
        Lt,
        Eq,
    }

    // input grammar
    // -------------
    pub struct Expr(pub ExprK, pub Type);
    pub enum ExprK {
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
    }
}

pub mod expose_alloca_grammar {
    type Int = i64;
    type Bool = bool;

    pub enum Type {
        Bool,
        Int,
        Void,
        Tuple(Vec<Type>),
    }

    pub enum UnaryOpKind {
        Neg,
        Not,
    }

    pub enum BinaryOpKind {
        And,
        Lt,
        Eq,
    }

    // input grammar
    // -------------
    pub struct Expr(pub ExprK, pub Type);
    pub enum ExprK {
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

        TupleLen(Box<Expr>),
        TupleRef(Box<Expr>, Int),
        TupleSet(Box<Expr>, Int, Box<Expr>),

        Collect(Int),
        Allocate(Int, Type),
        GlobalVar(String),
    }

    pub enum Value {
        Int(Int),
        Bool(Bool),
        Tuple(Vec<Value>),
    }
}

pub mod anf_grammar {
    type Int = i64;
    type Bool = bool;

    pub enum Atom {
        Int(Int),
        Bool(Bool),
        Var(String),
    }

    pub enum UnaryOpKind {
        Neg,
        Not,
    }

    pub enum BinaryOpKind {
        And,
        Lt,
        Eq,
    }

    pub enum Type {
        Bool,
        Int,
        Void,
        Tuple(Vec<Type>),
    }

    // input grammar
    // -------------
    pub enum Expr {
        Atom(Atom),

        Let(String, Box<Expr>, Box<Expr>),
        If(Box<Expr>, Box<Expr>, Box<Expr>),

        Read,
        UnaryOp(UnaryOpKind, Atom),
        BinaryOp(BinaryOpKind, Atom, Atom),

        TupleRef(Atom, Int),
        TupleSet(Atom, Int, Atom),

        Collect(Int),
        Allocate(Int, Type),
        GlobalVar(String),
    }

    pub enum Value {
        Int(Int),
        Bool(Bool),
        Tuple(Vec<Value>),
    }
}

pub mod c_grammar {
    type Int = i64;
    type Bool = bool;

    pub enum Atom {
        Int(Int),
        Bool(Bool),
        Var(String),
    }
    pub enum UnaryOpKind {
        Neg,
        Not,
    }

    pub enum BinaryOpKind {
        And,
        Lt,
        Eq,
    }

    pub enum Type {
        Bool,
        Int,
        Void,
        Tuple(Vec<Type>),
    }

    pub enum Expr {
        Atom(Atom),
        Read,
        UnaryOp(UnaryOpKind, Atom),
        BinaryOp(BinaryOpKind, Atom, Atom),
        Allocate(Int, Type),
        TupleRef(Atom, Int),
        TupleSet(Atom, Int, Atom),
        GlobalVar(String),
        Void,
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

pub mod x86var_grammar {
    type Int = i64;
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
    type Int = i64;
    pub enum Value {
        Int(Int),
        EFlag(CndCode),
    }

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

    pub enum CndCode {
        Eq,
        Lt,
        Gt,
    }

    pub enum Arg {
        Imm(Int),
        Var(String),
        Reg(Reg),
        ByteReg(ByteReg),
        Deref(Reg, Int),
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

pub mod x86reg_grammar {
    type Int = i64;
    pub enum Value {
        Int(Int),
        EFlag(CndCode),
    }

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

    pub enum CndCode {
        Eq,
        Lt,
        Gt,
    }

    pub enum Arg {
        Imm(Int),
        Reg(Reg),
        ByteReg(ByteReg),
        Deref(Reg, Int),
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
