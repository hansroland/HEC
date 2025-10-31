-- A Partial Evaluator
-- The compiler eagerly computes the parts of the program that do not
--    depend on any inputs, a process known as partial evaluation
--    (Jones, Gomard, and Sestoft 1993).

module LangInt.PartialEval (peModule) where

import LangInt.Syntax

peModule :: ModuleInt -> ModuleInt
peModule (ModuleInt body) = ModuleInt (peStmt <$> body)

peStmt :: Stmt -> Stmt
peStmt (PrintStmt expr) = PrintStmt $ peExpr expr
peStmt (ExprStmt expr)  = ExprStmt $ peExpr expr

peExpr :: Expr -> Expr
peExpr (BinOp Add lhs rhs) = peAdd (peExpr lhs) (peExpr rhs)
peExpr (BinOp Sub lhs rhs) = peSub (peExpr lhs) (peExpr rhs)
peExpr (UnaryOp USub e) = peNeg e
peExpr e = e

peAdd :: Expr -> Expr -> Expr
peAdd (Constant n1) (Constant n2) = Constant (n1 + n2)
peAdd e1 e2 = BinOp Add e1 e2

peSub :: Expr -> Expr -> Expr
peSub (Constant n1) (Constant n2) = Constant (n1 - n2)
peSub e1 e2 = BinOp Sub e1 e2

peNeg :: Expr -> Expr
peNeg (Constant n) = Constant $ negate n
peNeg s = s

