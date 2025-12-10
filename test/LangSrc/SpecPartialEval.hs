module LangSrc.SpecPartialEval (specPartialEval) where

import Test.Hspec
import LangSrc.Syntax
import LangSrc.Print
import LangSrc.PartialEval

runOptConst :: Expr -> String
runOptConst = qprintExpr . optimizeExpr


specPartialEval :: Spec
specPartialEval = do
  describe "Tests for module PartialEval" $ do
    it "runOptConst ex00" $ do
        (runOptConst ex00) `shouldBe` "(ExprInt 29)"
    it "runOptConst ex01" $ do
        (runOptConst ex01) `shouldBe` "(ExprBinOp Add (ExprInt 3) (ExprBinOp Add (ExprInt 3) (ExprCall getInt)))"
    it "runOptConst ex02" $ do
        (runOptConst ex02) `shouldBe` "(ExprBinOp Add (ExprCall getInt) (ExprUOp USub (ExprInt 8)))"

-- Parts of the syntax
ex00 :: Expr
ex00 = ExprBinOp Add (ExprBinOp Add (ExprInt 10) (ExprInt 20)) (ExprBinOp Sub (ExprInt 3) (ExprInt 4))

ex01 :: Expr
ex01 = ExprBinOp Add (ExprBinOp Add (ExprInt 1) (ExprInt 2)) (ExprBinOp Add (ExprInt 3) (ExprCall "getInt" []))

ex02 :: Expr
ex02 = ExprBinOp Add (ExprCall "getInt" []) (ExprUOp USub (ExprBinOp Add (ExprInt 5) (ExprInt 3)))


