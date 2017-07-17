// Original file: "AST.hs"
// File auto-generated using Corollary.

#[macro_use] use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Data::Char;
// use Numeric;
// use Text::PrettyPrint::HughesPJClass;

#[derive(Debug, Eq)]
pub struct Lifetime(pub String);


#[derive(Debug, Eq)]
pub struct TypeName(pub String);


#[derive(Debug, Eq)]
pub enum LitIntRepr {
    DecRepr,
    OctalRepr,
    HexRepr
}
pub use self::LitIntRepr::*;

#[derive(Debug, Eq)]
pub enum Lit {
    LitByteStr(String),
    LitByteChar(char),
    LitBool(bool),
    LitInt(isize, LitIntRepr, TypeName),
    LitFloat(String, TypeName)
}
pub use self::Lit::*;

#[derive(Debug, Eq)]
pub struct VarName(pub String);

#[derive(Debug)]
pub struct PathSegments(pub Vec<String>);


#[derive(Debug, Eq)]
pub enum Visibility {
    Public,
    Private
}
pub use self::Visibility::*;

#[derive(Debug, Eq)]
pub enum Mutable {
    Immutable,
    Mutable
}
pub use self::Mutable::*;

#[derive(Debug)]
pub enum Stmt {
    Stmt(Expr),
    Let(Mutable, VarName, Option<TypeName>, Option<Expr>),
    StmtItem(Vec<Attribute>, ItemKind)
}
pub use self::Stmt::*;

#[derive(Debug)]
pub struct Block(pub Vec<Stmt>, pub Option<Expr>);


pub fn pPrintBlock(_0: Doc, _1: Block) -> Doc {
    match (_0, _1) {
        (pre, Block([], e)) => {
            sep(vec![
                    __op_doc_concat(pre, text("{".to_string())),
                    nest(4, (maybe(empty, pPrint, e))),
                    text("}".to_string()),
                ])
        },
        (pre, Block(ss, e)) => {
            sep(vec![
                    __op_doc_concat(pre, text("{".to_string())),
                    nest(4, (maybe(empty, pPrint, e))),
                    text("}".to_string()),
                ])
        },
    }
}

#[derive(Debug)]
pub struct Attribute(pub String);


#[derive(Debug)]
pub struct Item(pub Vec<Attribute>, pub Visibility, pub ItemKind);


#[derive(Debug)]
pub enum FunctionAttribute {
    UnsafeFn,
    ExternABI(Option<String>)
}
pub use self::FunctionAttribute::*;

#[derive(Debug)]
pub enum ItemKind {
    Function(Vec<FunctionAttribute>, String, Vec<(Mutable, VarName, TypeName)>, TypeName, Block),
    Static(Mutable, VarName, TypeName, Expr),
    Struct(String, Vec<(String, TypeName)>),
    Extern(Vec<ExternItem>),
    Use(String),
    Enum(String, Vec<Enumerator>),
    CloneImpl(TypeName)
}
pub use self::ItemKind::*;

#[derive(Debug)]
pub enum ExternItem {
    ExternFn(String, Vec<(VarName, TypeName)>, bool, TypeName),
    ExternStatic(Mutable, VarName, TypeName)
}
pub use self::ExternItem::*;

#[derive(Debug)]
pub enum Enumerator {
    EnumeratorAuto(String),
    EnumeratorExpr(String, Expr)
}
pub use self::Enumerator::*;

#[derive(Debug)]
pub enum Expr {
    Lit(Lit),
    Var(VarName),
    Path(PathName),
    Index(Expr, Expr),
    ArrayExpr(Vec<Expr>),
    RepeatArray(Expr, Expr),
    StructExpr(String, Vec<(String, Expr)>, Option<Expr>),
    Call(Expr, Vec<Expr>),
    MethodCall(Expr, VarName, Vec<Expr>),
    Lambda(Vec<VarName>, Expr),
    Member(Expr, VarName),
    BlockExpr(Block),
    UnsafeExpr(Block),
    IfThenElse(Expr, Block, Block),
    Loop(Option<Lifetime>, Block),
    While(Option<Lifetime>, Expr, Block),
    For(Option<Lifetime>, VarName, Expr, Block),
    Break(Option<Lifetime>),
    Continue(Option<Lifetime>),
    Return(Option<Expr>),
    Neg(Expr),
    Deref(Expr),
    Not(Expr),
    Borrow(Mutable, Expr),
    Cast(Expr, TypeName),
    Mul(Expr, Expr),
    Div(Expr, Expr),
    Mod(Expr, Expr),
    Add(Expr, Expr),
    Sub(Expr, Expr),
    ShiftL(Expr, Expr),
    ShiftR(Expr, Expr),
    And(Expr, Expr),
    Xor(Expr, Expr),
    Or(Expr, Expr),
    CmpLT(Expr, Expr),
    CmpGT(Expr, Expr),
    CmpLE(Expr, Expr),
    CmpGE(Expr, Expr),
    CmpEQ(Expr, Expr),
    CmpNE(Expr, Expr),
    LAnd(Expr, Expr),
    LOr(Expr, Expr),
    Range(Expr, Expr),
    Assign(Expr, AssignOp, Expr)
}
pub use self::Expr::*;

#[derive(Debug)]
pub enum AssignOp {
    __id_3a3d,
    __id_3a2b3d,
    __id_3a2d3d,
    __id_3a2a3d,
    __id_3a2f3d,
    __id_3a253d,
    __id_3a263d,
    __id_3a7c3d,
    __id_3a5e3d,
    __id_3a3c3c3d,
    __id_3a3e3e3d
}
pub use self::AssignOp::*;

#[derive(Debug)]
pub enum ExprPosition {
    TopExpr,
    LeftExpr,
    RightExpr
}
pub use self::ExprPosition::*;



