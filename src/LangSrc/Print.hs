-- Print - Module with Print interpreters
module LangSrc.Print where

import LangSrc.Syntax

-- Define a pretty printer. It produces an easy and quick string of a sentence.
--    (This is more ore less the input to the compiler...)
pprint :: Progr -> String
pprint (Progr stmts) = concat $ pprintStmt <$> stmts

pprintStmt :: Stmt -> String
pprintStmt (StmtPrint e) = concat ["print ", pprintExpr e]
pprintStmt (StmtExpr e) = pprintExpr e

pprintExpr :: Expr -> String
pprintExpr (ExprInt n)
          | n >= 0    = show n
          | otherwise = concat ["(", show n, ")"]
pprintExpr (ExprBinOp Add e1 e2) = concat [ "(", pprintExpr e1, " + ", pprintExpr e2, ")" ]
pprintExpr (ExprBinOp Sub e1 e2) = concat [ "(", pprintExpr e1, " - ", pprintExpr e2, ")" ]
pprintExpr (ExprUOp USub e)      = concat ["-", pprintExpr e]
pprintExpr (ExprCall name args)  = concat ["Call ", name, prtargs]
   where prtargs = concat $ pprintExpr <$> args

-- Quick Print
-- The quick print should print the expression in the abstract syntax
-- The input is supposed to be processed by the Language Parser
qprint :: Progr -> String
qprint (Progr stmts) = concat $ qprintStmt <$> stmts

qprintStmt :: Stmt -> String
qprintStmt (StmtPrint e) = concat ["print ", qprintExpr e]
qprintStmt (StmtExpr e) = qprintExpr e

qprintExpr :: Expr -> String
qprintExpr (ExprInt n) = concat ["(ExprInt ", show n, ")"]
qprintExpr (ExprBinOp Add e1 e2) = concat [ "(ExprBinOp Add ", qprintExpr e1, " ", qprintExpr e2, ")" ]
qprintExpr (ExprBinOp Sub e1 e2) = concat [ "(ExprBinOp Sub ", qprintExpr e1, " ", qprintExpr e2, ")" ]
qprintExpr (ExprUOp USub e)      = concat ["(ExprUOp USub ", qprintExpr e, ")"]
qprintExpr (ExprCall name args)  = concat ["(ExprCall ", name, prtargs, ")"]
   where prtargs = concat $ qprintExpr <$> args


