

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Type {
    N,
    R,
    Bool,
    Z,
    Q,
    Func {
        args: Vec<Type>,
        ret: Box<Type>
    }
    // Range { start: Option<f64>, end: Option<f64>, inclusive_start: bool, inclusive_end: bool },
    // Set(Vec<f64>),
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    
    Equal
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    Ident(String),
    FuncCall { name: String, args: Vec<Expr> },
    BinOp { left: Box<Expr>, right: Box<Expr>, op: Operator },
    IfElse {cond: Box<Expr>, then: Box<Expr>, otherwise: Box<Expr>},
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: String,
    pub args: Vec<Arg>,
    pub ret_type: Type,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    FuncDef(FuncDef),
    Ret(Expr),
    Const {name: String, const_type: Type, value: Expr},
}

#[derive(Debug, Clone)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}
