{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Evaluator (aka Language Interpreter) for the first languagne define in LangInt.
--
module LangInt.Eval  where

import LangInt.Syntax
import Control.Monad.IO.Class

instance Expr (IO Int) where
    type ExprTy (IO Int) Int = IO Int
    int x = pure x
    usub e = negate <$> e
    add = liftA2 (+)
    sub = liftA2 (-)
    getInt p = do
        liftIO $ putStrLn p
        str <- getLine
        return $ stringToInt str

-- How to call the interpreter?
-- exp1 @(IO Int) or  eval eval (exp1 @(IO Int))
-- eval :: Repr (IO Int) Int -> Repr (IO Int) Int
eval :: ExprTy (IO Int) Int -> ExprTy (IO Int) Int
eval  x = id x

stringToInt :: String -> Int
stringToInt s = read s

instance Stmt (IO Int) where
    type StmtTy (IO Int) ()  = IO ()
    qprint eo = do
        e <- eo
        liftIO $ putStrLn (show e)

exp1 :: forall f. Expr f => ExprTy f Int
exp1 = int @f 1

exp2 :: forall f. (Expr f, Stmt f) => StmtTy f ()
exp2 = qprint @f (add @f (int @f 1) (int @f 6))

exp3 :: forall f. (Expr f, Stmt f) => StmtTy f ()
exp3 = qprint @f (add @f (int @f 1) (int @f 22))

