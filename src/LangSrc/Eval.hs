-- Evaluator (aka Language Interpreter) for the LangVar languagne defined in LangSrc.
--
module LangSrc.Eval (evalProgr, evalStmt) where

import LangSrc.Syntax
import LangSrc.Print (pprintExpr)

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Maybe ( fromJust )

data EvalState = EvalState (Map String Int)
type EvalMonad a = StateT EvalState IO a

initialState :: EvalState
initialState = EvalState Map.empty

evalProgr :: Progr -> IO ()
evalProgr (Progr stmts) = do
    _ <- execStateT (sequence_ (map evalStmt stmts)) initialState
    pure ()

evalStmt :: Stmt -> EvalMonad ()
evalStmt (StmtPrint e) = do
    n <- evalExpr e
    liftIO $ putStrLn $ show n
evalStmt (StmtExpr e) = do
    _ <- evalExpr e
    pure ()
evalStmt (StmtAssign var e) = do
    (EvalState vars) <- get
    n <- evalExpr e
    let nVars = Map.insert var n vars
    put $ EvalState nVars
    return ()
-- This is a classic interpreter for a standard ADT Syntax tree.
-- It runs in the EvalMonad and allows us to keep IO out of the Syntax definition
-- type classes used to define our language.
evalExpr :: Expr -> EvalMonad Int
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
evalExpr (ExprCall "getInt" []) = liftIO $
    putStr "getInt: Enter integer" >> ((read <$> getLine) :: IO Int)
evalExpr (ExprVar var) = do                 -- Lookup int value of variable
        (EvalState vars) <- get
        let mb = Map.lookup var vars
        let n = fromJust mb
        pure n
evalExpr e = error ("Error on evaluating expression: " ++ pprintExpr e)
