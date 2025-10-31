-- Evaluator (aka Language Interpreter) for the first languagne define in LangInt.
--
module LangInt.Eval (evalModule) where

import LangInt.Syntax

evalModule :: ModuleInt -> IO ()
evalModule (ModuleInt stmts ) =  mapM_  evalStmt stmts

evalStmt :: Stmt -> IO (Int)
evalStmt (PrintStmt e) = do
    n <- evalExpr e
    putStrLn $ show n
    return 0
evalStmt (ExprStmt e) = evalExpr e

-- This is a classic interpreter for a standard ADT Syntax tree.
-- It has nothing to do with tagless final.(*)
-- It runs in the IO Monad and allows us to keep IO out of the
-- type classes used to define our language.
evalExpr :: Expr -> IO Int
evalExpr (Constant n) = pure n
evalExpr (UnaryOp USub expr) = do
    e <- evalExpr expr
    pure $ negate e
evalExpr (BinOp op exp1 exp2) = do
    e1 <- evalExpr exp1
    e2 <- evalExpr exp2
    pure (case op of
           Add -> e1 + e2
           Sub -> e1 - e2)
evalExpr (Call "getInt" []) = putStr "getInt: Enter integer" >> stringToInt <$> getLine
evalExpr e = error ("Error on evaluating expression: " ++ show e)

stringToInt :: String -> Int
stringToInt s = read s