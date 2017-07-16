module Test ()
where

castTo :: CType -> Result -> Rust.Expr
castTo target source = doshit
    where
    incdec returnOld assignop = do
        expr' <- interpretExpr True expr
        compound node returnOld demand assignop expr' Result
            { resultType = IsInt Signed (BitWidth 32)
            , resultMutable = Rust.Immutable
            , result = 1
            }
    simple f = do
        expr' <- interpretExpr True expr
        let ty' = intPromote (resultType expr')
        return Result
            { resultType = ty'
            , resultMutable = Rust.Immutable
            , result = f (castTo ty' expr')
            }