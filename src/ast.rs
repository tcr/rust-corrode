// Original file: "AST.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Data::Char;
// use Numeric;
// use Text::PrettyPrint::HughesPJClass;

#[derive(Debug, Eq, PartialEq)]
pub struct Lifetime(pub String);


#[derive(Debug, Eq, PartialEq)]
pub struct TypeName(pub String);


#[derive(Debug, Eq, PartialEq)]
pub enum LitIntRepr {
    DecRepr,
    OctalRepr,
    HexRepr,
}
pub use self::LitIntRepr::*;

#[derive(Debug, Eq, PartialEq)]
pub enum Lit {
    LitByteStr(String),
    LitByteChar(char),
    LitBool(bool),
    LitInt(isize, LitIntRepr, TypeName),
    LitFloat(String, TypeName),
}
pub use self::Lit::*;

#[derive(Debug, Eq, PartialEq)]
pub struct VarName(pub String);

#[derive(Debug)]
pub struct PathSegments(pub Vec<String>);


#[derive(Debug, Eq, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}
pub use self::Visibility::*;

#[derive(Debug, Eq, PartialEq)]
pub enum Mutable {
    Immutable,
    Mutable,
}
pub use self::Mutable::*;

#[derive(Debug)]
pub enum Stmt {
    Stmt(Expr),
    Let(Mutable, VarName, Option<TypeName>, Option<Expr>),
    StmtItem(Vec<Attribute>, ItemKind),
}
pub use self::Stmt::*;

#[derive(Debug)]
pub struct Block(pub Vec<Stmt>, pub Option<Box<Expr>>);


pub fn pPrintBlock(_0: Doc, _1: Block) -> Doc {
    match (_0, _1) {
        (pre, Block([], e)) => {
            sep(vec![
                __op_doc_concat(pre, text("{".to_string())),
                nest(4, e.map(pPrint).unwrap_or(vec![])),
                text("}".to_string()),
            ])
        }
        (pre, Block(ss, e)) => {
            sep(vec![
                __op_doc_concat(pre, text("{".to_string())),
                nest(4, e.map(pPrint).unwrap_or(vec![])),
                text("}".to_string()),
            ])
        }
    }
}

#[derive(Debug)]
pub struct Attribute(pub String);


#[derive(Debug)]
pub struct Item(pub Vec<Attribute>, pub Visibility, pub ItemKind);


#[derive(Debug)]
pub enum FunctionAttribute {
    UnsafeFn,
    ExternABI(Option<String>),
}
pub use self::FunctionAttribute::*;

#[derive(Debug)]
pub enum ItemKind {
    Function(
        Vec<FunctionAttribute>,
        String,
        Vec<(Mutable, VarName, TypeName)>,
        TypeName,
        Block,
    ),
    Static(Mutable, VarName, TypeName, Expr),
    Struct(String, Vec<(String, TypeName)>),
    Extern(Vec<ExternItem>),
    Use(String),
    Enum(String, Vec<Enumerator>),
    CloneImpl(TypeName),
}
pub use self::ItemKind::*;

#[derive(Debug)]
pub enum ExternItem {
    ExternFn(String, Vec<(VarName, TypeName)>, bool, TypeName),
    ExternStatic(Mutable, VarName, TypeName),
}
pub use self::ExternItem::*;

#[derive(Debug)]
pub enum Enumerator {
    EnumeratorAuto(String),
    EnumeratorExpr(String, Expr),
}
pub use self::Enumerator::*;

#[derive(Debug)]
pub enum Expr {
    Lit(Lit),
    Var(VarName),
    Path(PathSegments),
    Index(Box<Expr>, Box<Expr>),
    ArrayExpr(Vec<Expr>),
    RepeatArray(Box<Expr>, Box<Expr>),
    StructExpr(String, Vec<(String, Expr)>, Option<Box<Expr>>),
    Call(Box<Expr>, Vec<Expr>),
    MethodCall(Box<Expr>, VarName, Vec<Expr>),
    Lambda(Vec<VarName>, Box<Expr>),
    Member(Box<Expr>, VarName),
    BlockExpr(Block),
    UnsafeExpr(Block),
    IfThenElse(Box<Expr>, Block, Block),
    Loop(Option<Lifetime>, Block),
    While(Option<Lifetime>, Box<Expr>, Block),
    For(Option<Lifetime>, VarName, Box<Expr>, Block),
    Break(Option<Lifetime>),
    Continue(Option<Lifetime>),
    Return(Option<Box<Expr>>),
    Neg(Box<Expr>),
    Deref(Box<Expr>),
    Not(Box<Expr>),
    Borrow(Mutable, Box<Expr>),
    Cast(Box<Expr>, TypeName),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    ShiftL(Box<Expr>, Box<Expr>),
    ShiftR(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    CmpLT(Box<Expr>, Box<Expr>),
    CmpGT(Box<Expr>, Box<Expr>),
    CmpLE(Box<Expr>, Box<Expr>),
    CmpGE(Box<Expr>, Box<Expr>),
    CmpEQ(Box<Expr>, Box<Expr>),
    CmpNE(Box<Expr>, Box<Expr>),
    LAnd(Box<Expr>, Box<Expr>),
    LOr(Box<Expr>, Box<Expr>),
    Range(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, AssignOp, Box<Expr>),
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
    __id_3a3e3e3d,
}
pub use self::AssignOp::*;

#[derive(Debug)]
pub enum ExprPosition {
    TopExpr,
    LeftExpr,
    RightExpr,
}
pub use self::ExprPosition::*;
