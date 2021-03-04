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

pub mod expose_alloca {
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
