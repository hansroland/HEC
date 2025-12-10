-- Abstract Syntax Tree for the language to compile

module LangSrc.Syntax where

data BinOp = Add | Sub
    deriving (Show, Eq)

data UnaryOp = USub
    deriving (Show, Eq)

data Expr =
    ExprInt Int
    | ExprBinOp BinOp Expr Expr
    | ExprUOp UnaryOp Expr
    | ExprCall Name [Expr]
  deriving (Eq, Show)

type Name = String

data Stmt = StmtPrint Expr
          | StmtExpr  Expr
  deriving (Eq, Show)

data Progr = Progr [Stmt]
  deriving (Eq, Show)
