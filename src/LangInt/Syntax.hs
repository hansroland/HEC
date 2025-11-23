{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}

-- This module implements the L_Int Syntax.
-- See python-book fig 1.1 p 5.

module LangInt.Syntax where

class Expr f where
    type ExprTy f a
    int :: Int -> ExprTy f Int
    getInt :: String -> ExprTy f Int
    usub :: ExprTy f Int -> ExprTy f Int           -- unary subtraction aka negation
    add :: ExprTy f Int -> ExprTy f Int -> ExprTy f Int
    sub :: ExprTy f Int -> ExprTy f Int -> ExprTy f Int
    -- missing: `(exp)`

class (Expr f) => Stmt f where
    type StmtTy f a
    -- stmtexpr :: ExprTy f Int -> StmtTy f Int
    qprint   :: ExprTy f Int -> StmtTy f ()
    -- For now, I don't support expressions as statements. The program needs to have an output.

-- I don't add the module class, we defer multiple statements until we have assignement.



