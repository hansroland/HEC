module LangSrc.SpecEval (specEval) where

import Test.Hspec
import LangSrc.Syntax
import LangSrc.Eval(evalStmt)

-- To redirect stdin see:
-- https://stackoverflow.com/questions/76683594/in-haskell-how-can-i-interact-with-stdin-of-an-io

import System.IO.Silently

-- Run the test in the IO Monad
evaltest :: Stmt -> IO String
evaltest e = do
    output <- capture_ $ evalStmt e
    pure output

specEval :: Spec
specEval = do
  describe "Tests for module LangInt.Eval" $ do
    it "evaltest testLit01" $ do
        evaltest (testLit01) `shouldReturn` "34\n"
    it "evaltest testNeg01" $ do
        evaltest (testNeg01) `shouldReturn` "-42\n"
    it "evaltest testNeg02" $ do
        evaltest (testNeg02) `shouldReturn` "5\n"
    it "evaltest testAdd01" $ do
        evaltest (testAdd01) `shouldReturn` "42\n"
    it "evaltest testAdd02" $ do
        evaltest (testAdd02) `shouldReturn` "10\n"
    it "evaltest testSub01" $ do
        evaltest (testSub01) `shouldReturn` "-26\n"
    it "evaltest testSub02" $ do
        evaltest (testSub02) `shouldReturn` "-2\n"

testLit01 :: Stmt
testLit01 = StmtPrint  (ExprInt 34)

testNeg01 :: Stmt
testNeg01 = StmtPrint  (ExprUOp USub  (ExprInt 42))

testNeg02 :: Stmt
testNeg02 = StmtPrint  (ExprBinOp Add (ExprInt 8) (ExprUOp USub ((ExprBinOp Add (ExprInt 1) (ExprInt 2)))))

testAdd01 :: Stmt
testAdd01 = StmtPrint  (ExprBinOp Add (ExprInt 8) (ExprInt 34))

testAdd02 :: Stmt
testAdd02 = StmtPrint   (ExprBinOp Add (ExprBinOp Add (ExprInt 1) (ExprInt 2)) (ExprBinOp Add (ExprInt 3) (ExprInt 4)))

testSub01 :: Stmt
testSub01 = StmtPrint   (ExprBinOp Sub (ExprInt 8) (ExprInt 34))

testSub02 :: Stmt
testSub02 = StmtPrint  (ExprBinOp Add (ExprBinOp Sub (ExprInt 1) (ExprInt 2)) (ExprBinOp Sub (ExprInt 3) (ExprInt 4)))
