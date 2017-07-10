// Original file: "Idiomatic.hs"
// File auto-generated using Corollary.

#[macro_use] use corollary_support::*;

use ast as Rust;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Language::Rust::AST;

pub fn unsnoc<a>(mut _0: Vec<a>) -> Option<(Vec<a>, a)> {
    if _0.len() == 0 {
        None
    } else {
        let x = _0.remove(0);
        let xs = _0;
        match unsnoc(xs) {
            Some((a, b)) => Some(({
                let v = vec![x];
                x.extend(a);
                x
            }, b)),
            None => Some((vec![], x))
        }
    }
}

pub fn tailExpr(_0: Rust::Expr) -> Option<Option<Rust::Expr>> {
    match _0 {
        Rust::Return(e) => {
            Some(e)
        },
        Rust::BlockExpr(b) => {
            Some (Some (Rust::BlockExpr (tailBlock(b))))
        },
        Rust::IfThenElse(c, t, f) => {
            Some (Some (Rust::IfThenElse(c, tailBlock(t), tailBlock(f))))
        },
        _ => {
            None
        },
    }
}

pub fn tailBlock(_0: Rust::Block) -> Rust::Block {
    match _0 {
        Rust::Block(b, Some(/* TODO ViewPattern */ tailExpr)) => {
            Rust::Block(b, e)
        },
        Rust::Block(/* TODO ViewPattern */ unsnoc, None) => {
            Rust::Block(b, e)
        },
        b => {
            Rust::Block(b, e)
        },
    }
}

pub fn itemIdioms(_0: Rust::Item) -> Rust::Item {
    match _0 {
        Rust::Item(attrs, vis, Rust::Function(fattrs, name, formals, ret, b)) => {
            Rust::Item(attrs, vis, (Rust::Function(fattrs, name, formals, ret, (tailBlock(b)))))
        },
        i => {
            i
        },
    }
}



