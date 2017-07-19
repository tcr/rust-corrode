module Test ()
where

promoteArg :: Result -> Rust.Expr
promoteArg r = alpha
    where
        output = runST (evalRWST (runExceptT (mapM_ perDecl decls)) initFlow initState)