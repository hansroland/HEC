{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This is the pretty printer for the LangInt syntax

module LangInt.Print where

import LangInt.Syntax

newtype QP  = QP String
    deriving (Show, Eq)
unQP :: QP  -> String
unQP (QP x) = x

instance Expr QP where
    type ExprTy QP Int = QP
    int n = QP $ show n
    getInt p1 = QP (concat ["(getInt ",  "\"", p1, "\")"])
          -- unary subtraction aka negation
    usub e = QP $ concat ["-", unQP e]
    add e1 e2 = QP ( concat["(", unQP e1, " + ", unQP e2, ")" ] )
    sub e1 e2 = QP ( concat["(", unQP e1, " - ", unQP e2, ")" ] )

instance Stmt QP where
    type StmtTy QP () = QP
    type StmtTy QP Int = QP
    qprint e = QP $ concat ["qprint ", unQP e]

