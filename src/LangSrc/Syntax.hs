-- Abstract Syntax tree for LangVar

module LangSrc.Syntax where

data BinOp = Add | Sub
    deriving (Show, Eq)

data UnaryOp = USub
    deriving (Show, Eq)

-- Expressions
data Expr =
    ExprInt !Int
    | ExprBinOp !BinOp !Expr !Expr
    | ExprUOp !UnaryOp !Expr
    | ExprCall !Name ![Expr]
    | ExprVar !Name
  deriving (Eq, Show)

type Name = String

-- A Statement of our object language
data Stmt = StmtPrint  !Expr
          | StmtAssign !Name !Expr
          | StmtExpr   !Expr
  deriving (Eq, Show)

-- A program of our object language
data Progr = Progr ![Stmt]
  deriving (Eq, Show)
