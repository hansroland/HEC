{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}

module SpecPrint (specPrint) where

import Test.Hspec
import LangInt.Syntax
import LangInt.Print

-- Tests for the Print interpreters
specPrint :: Spec
specPrint = do
  describe "Tests for module Print - quickprint" $ do
    it "testLit00" $ do
        (testLit00) @QP  `shouldBe` QP "34"
    it "testLit01" $ do
        (testLit01) @QP  `shouldBe` QP "qprint 34"
    it "testNeg01" $ do
        (testNeg01) @QP `shouldBe` QP "qprint -42"
    it "testNeg02" $ do
        (testNeg02) @QP `shouldBe` QP "qprint (8 + -(1 + 2))"
    it "testAdd01" $ do
        (testAdd01) @QP `shouldBe` QP "qprint (8 + 34)"
    it "testAdd02" $ do
        (testAdd02) @QP `shouldBe` QP "qprint ((1 + 2) + (3 + 4))"
    it "testSub01" $ do
        (testSub01) @QP `shouldBe` QP "qprint (8 - 34)"
    it "testSub02" $ do
        (testSub02) @QP `shouldBe` QP "qprint ((1 - 2) - (3 - 4))"

testLit00 :: forall f. (Expr f) => ExprTy f Int
testLit00 = (int @f 34)

testLit01 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testLit01 = qprint @f (int @f 34)

testNeg01 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testNeg01 = qprint @f (usub @f (int @f  42))

testNeg02 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testNeg02 = qprint @f (add @f (int @f 8) (usub  @f ((add  @f (int @f 1) (int @f 2)))))

testAdd01 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testAdd01 = qprint @f (add @f (int @f 8) (int @f 34))

testAdd02 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testAdd02 = qprint @f (add @f (add  @f (int @f 1) (int @f 2)) (add  @f(int @f 3) (int @f 4)))

testSub01 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testSub01 = qprint @f (sub @f (int @f 8) (int @f  34))

testSub02 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testSub02 = qprint @f (sub @f (sub  @f (int @f  1) (int @f  2)) (sub @f (int @f 3) (int @f 4)))
