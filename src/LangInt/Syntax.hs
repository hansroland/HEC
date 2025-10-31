-- Abstract Syntax Tree for LangInt

module LangInt.Syntax where

data BinOpCode = Add | Sub
    deriving (Show, Eq)

data UnaryOpCode = USub
    deriving (Show, Eq)

data Expr =
    Constant Int
    | BinOp BinOpCode Expr Expr
    | UnaryOp UnaryOpCode Expr
    | Call Name [Expr]
  deriving (Eq, Show)

type Name = String

data Stmt = PrintStmt Expr
          | ExprStmt  Expr
  deriving (Eq, Show)

data ModuleInt = ModuleInt [Stmt]
  deriving (Eq, Show)

