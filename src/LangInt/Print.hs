-- Print - Module with Print interpreters
module LangInt.Print where

import LangInt.Syntax

-- The quick print should print th expression in the concrete syntax
-- The input is supposed to be processed by the Language Parser

-- Define a quick printer. It produces an easy and quick string of a sentence.

qprint :: ModuleInt -> String
qprint (ModuleInt stmts) = concat $ qprintStmt <$> stmts

qprintStmt :: Stmt -> String
qprintStmt (PrintStmt e) = "qprint " ++ qprintExpr e ++ "\n"
qprintStmt (ExprStmt e) = qprintExpr e ++ "\n"

qprintExpr :: Expr -> String
qprintExpr (Constant n)
          | n >= 0    = show n
          | otherwise = concat ["(", show n, ")"]
qprintExpr (BinOp Add e1 e2) = concat [ "(", qprintExpr e1, " + ", qprintExpr e2, ")" ]
qprintExpr (BinOp Sub e1 e2) = concat [ "(", qprintExpr e1, " - ", qprintExpr e2, ")" ]
qprintExpr (UnaryOp USub e)  = concat ["-", qprintExpr e]
qprintExpr (Call name args)  = concat ["Call ", name, prtargs]
   where prtargs = concat $ qprintExpr <$> args



{-}

-- Define a debug printer. It shwos functions and values
newtype StrDebug = StrDebug {undebug :: String}
    deriving (Show, Semigroup, Monoid)

instance Syn1 StrDebug where
    lit n = StrDebug $ concat [ "(lit ", show n, ")"]
    neg e = StrDebug $ concat ["neg (", undebug e , ")"]
    add e1 e2 = StrDebug $ concat ["(add ", undebug e1, " ", undebug e2, ")"]
    sub e1 e2 = StrDebug $ concat ["(sub ", undebug e1, " ", undebug e2, ")"]
    getint s = StrDebug $ concat ["getint(", s, ") "]
    qprt e = StrDebug $ concat [ "qprt ", undebug e ]
strDebug :: StrDebug -> StrDebug
strDebug = id

-}
