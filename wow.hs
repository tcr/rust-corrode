module Test ()
where

promoteArg :: Result -> Rust.Expr
promoteArg r = case resultType r of
    IsFloat _ -> castTo (IsFloat 64) r
    IsArray mut _ el -> castTo (IsPtr mut el) r
    ty -> castTo (intPromote ty) r