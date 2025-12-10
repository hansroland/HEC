-- Evaluator (aka Language Interpreter) for the first language defined in LangSrc (aka LangInt).
--
module LangSrc.Eval (evalProgr, evalStmt) where

import LangSrc.Syntax

evalProgr :: Progr -> IO ()
evalProgr (Progr stmts ) =  mapM_ evalStmt stmts

evalStmt :: Stmt -> IO (Int)
evalStmt (StmtPrint e) = do
    n <- evalExpr e
    putStrLn $ show n
    return 0
evalStmt (StmtExpr e) = evalExpr e

-- This is a classic interpreter for a standard ADT Syntax tree.
-- It runs in the IO Monad and allows us to keep IO out of the Syntax definition.
evalExpr :: Expr -> IO Int
evalExpr (ExprInt n) = pure n
evalExpr (ExprUOp USub expr) = do
    e <- evalExpr expr
    pure $ negate e
evalExpr (ExprBinOp op exp1 exp2) = do
    e1 <- evalExpr exp1
    e2 <- evalExpr exp2
    pure (case op of
           Add -> e1 + e2
           Sub -> e1 - e2)
evalExpr (ExprCall "getInt" []) = putStr "getInt: Enter integer" >> ((read  <$> getLine) :: IO Int)
evalExpr e = error ("Error on evaluating expression: " ++ show e)
