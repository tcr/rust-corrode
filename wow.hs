module Test ()
where

promoteArg :: Result -> Rust.Expr
promoteArg r = alpha
    where
        mkIf c' t' f' = Rust.IfThenElse c' (Rust.Block [] (Just t')) (Rust.Block [] (Just f'))