use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Eq => "==",
            BinOp::NotEq => "!=",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
            BinOp::LtEq => "<=",
            BinOp::GtEq => ">=",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntLiteral(i64),
    Var(String),
    BinOp {
        left: Box<Expr>,
        right: Box<Expr>,
        op: BinOp,
    },
    UnaryNeg(Box<Expr>),
    Assign {
        name: String,
        value: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum ForInit {
    VarDecl { name: String, init: Expr },
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    VarDecl {
        name: String,
        init: Expr,
    },
    Block(Block),
    If {
        cond: Expr,
        then: Block,
        else_: Option<Block>,
    },
    For {
        init: Option<ForInit>,
        cond: Expr,
        step: Expr,
        body: Block,
    },
    Return(Expr),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub name: String,
    pub body: Block,
}
