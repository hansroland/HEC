-- A Partial Evaluator
-- The compiler eagerly computes the parts of the program that do not
--    depend on any inputs, a process known as partial evaluation
--    (Jones, Gomard, and Sestoft 1993).

module LangSrc.PartialEval (
    optimizeProg
    , optimizeExpr
    )

  where

import LangSrc.Syntax

-- Optimize Constants:
--   Replace: ExprBinOp Add (ExprInt x) (ExprInt y) => (ExprInt (x+y))
--            ExprBinOp Sub (ExprInt x) (ExprInt y) => (ExprInt (x-y))
optimizeProg :: Progr -> Progr
optimizeProg = optUSubProgr . optConstProgr

optimizeExpr :: Expr -> Expr
optimizeExpr = optUSubExpr . optConstExpr

optConstProgr :: Progr -> Progr
optConstProgr (Progr body) = Progr (optConstStmt <$> body)

optConstStmt :: Stmt -> Stmt
optConstStmt (StmtPrint expr) = StmtPrint $ optConstExpr expr
optConstStmt (StmtExpr expr)  = StmtExpr $ optConstExpr expr

optConstExpr :: Expr -> Expr
optConstExpr (ExprBinOp Add lhs rhs) = optConstAdd (optConstExpr lhs) (optConstExpr rhs)
optConstExpr (ExprBinOp Sub lhs rhs) = optConstSub (optConstExpr lhs) (optConstExpr rhs)
optConstExpr (ExprUOp USub e) = (ExprUOp USub (optConstExpr e))
optConstExpr e = e

optConstAdd :: Expr -> Expr -> Expr
optConstAdd (ExprInt n1) (ExprInt n2) = ExprInt (n1 + n2)
optConstAdd e1 e2 = ExprBinOp Add e1 e2

optConstSub :: Expr -> Expr -> Expr
optConstSub (ExprInt n1) (ExprInt n2) = ExprInt (n1 - n2)
optConstSub e1 e2 = ExprBinOp Sub e1 e2


-- Optimizing the Unary Sub operation (aka negation)
--   Remove double negations
--   Replace: ExprUOp USub (ExprInt n) => (ExprInt -n)
optUSubProgr :: Progr -> Progr
optUSubProgr (Progr body) = Progr (optUSubStmt <$> body)

optUSubStmt :: Stmt -> Stmt
optUSubStmt (StmtPrint expr) = StmtPrint $ optUSubExpr expr
optUSubStmt (StmtExpr expr)  = StmtExpr $ optUSubExpr expr

optUSubExpr :: Expr -> Expr
optUSubExpr (ExprUOp USub (ExprInt n)) = ExprInt (-n)
optUSubExpr (ExprUOp USub (ExprUOp USub e)) = e
optUSubExpr e = e
