module LangSrc.SpecPrint (specPrint) where

import Test.Hspec
import LangSrc.Syntax
import LangSrc.Print

-- Tests for the LangInt.Print interpreters
specPrint :: Spec
specPrint = do
  describe "Tests for module Print - pretty-print" $ do
    it "testLit01" $ do
        (pprintStmt testLit01) `shouldBe` "print 34"
    it "testNeg01" $ do
        (pprintStmt testNeg01)`shouldBe` "print -42"
    it "testNeg02" $ do
        (pprintStmt testNeg02)`shouldBe` "print (8 + -(1 + 2))"
    it "testAdd01" $ do
        (pprintStmt testAdd01) `shouldBe` "print (8 + 34)"
    it "testAdd02" $ do
        (pprintStmt testAdd02) `shouldBe` "print ((1 + 2) + (3 + 4))"
    it "testSub01" $ do
        (pprintStmt testSub01) `shouldBe` "print (8 - 34)"
    it "testSub02" $ do
        (pprintStmt testSub02) `shouldBe` "print ((1 - 2) - (3 - 4))"

  describe "Tests for module Print - quick-print" $ do
    it "testLit01" $ do
        (qprintStmt testLit01) `shouldBe` "print (ExprInt 34)"
    it "testNeg01" $ do
        (qprintStmt testNeg01)`shouldBe` "print (ExprUOp USub (ExprInt 42))"
    it "testNeg02" $ do
        (qprintStmt testNeg02)`shouldBe`
            "print (ExprBinOp Add (ExprInt 8) (ExprUOp USub (ExprBinOp Add (ExprInt 1) (ExprInt 2))))"
    it "testAdd01" $ do
        (qprintStmt testAdd01) `shouldBe` "print (ExprBinOp Add (ExprInt 8) (ExprInt 34))"
    it "testAdd02" $ do
        (qprintStmt testAdd02) `shouldBe`
            "print (ExprBinOp Add (ExprBinOp Add (ExprInt 1) (ExprInt 2)) (ExprBinOp Add (ExprInt 3) (ExprInt 4)))"
    it "testSub01" $ do
        (qprintStmt testSub01) `shouldBe` "print (ExprBinOp Sub (ExprInt 8) (ExprInt 34))"
    it "testSub02" $ do
        (qprintStmt testSub02) `shouldBe`
            "print (ExprBinOp Sub (ExprBinOp Sub (ExprInt 1) (ExprInt 2)) (ExprBinOp Sub (ExprInt 3) (ExprInt 4)))"


testLit01 :: Stmt
testLit01 = StmtPrint (ExprInt 34)

testNeg01 :: Stmt
testNeg01 = StmtPrint (ExprUOp USub(ExprInt  42))

testNeg02 :: Stmt
testNeg02 = StmtPrint (ExprBinOp Add (ExprInt 8) (ExprUOp USub ((ExprBinOp Add  (ExprInt 1) (ExprInt 2)))))

testAdd01 :: Stmt
testAdd01 = StmtPrint (ExprBinOp Add (ExprInt 8) (ExprInt 34))

testAdd02 :: Stmt
testAdd02 = StmtPrint (ExprBinOp Add (ExprBinOp Add  (ExprInt 1) (ExprInt 2)) (ExprBinOp Add (ExprInt 3) (ExprInt 4)))

testSub01 :: Stmt
testSub01 = StmtPrint (ExprBinOp Sub (ExprInt 8) (ExprInt 34))

testSub02 :: Stmt
testSub02 = StmtPrint (ExprBinOp Sub (ExprBinOp Sub  (ExprInt 1) (ExprInt 2)) (ExprBinOp Sub (ExprInt 3) (ExprInt 4)))
