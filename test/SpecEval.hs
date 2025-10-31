module SpecEval (specEval) where

import Test.Hspec
import LangInt.Syntax
import LangInt.Eval

import System.IO.Silently

-- This is very hacky! But we get always 2 lines!!!
-- I don't know yet why !!!
evaltest :: ModuleInt -> IO String
evaltest m = do
    (output, _) <- capture $ evalModule m
    pure $ checkdata $ lines output
  where
    checkdata :: [String] -> String
    checkdata (l : _)  = l
    checkdata []       = "No output on stdout found"

specEval :: Spec
specEval = do
  describe "Tests for module Eval" $ do
    it "evaltest testLit01" $ do
        evaltest testLit01 `shouldReturn` "34"
    it "evaltest testNeg01" $ do
        evaltest testNeg01 `shouldReturn` "-42"
    it "evaltest testNeg02" $ do
        evaltest testNeg02 `shouldReturn` "5"
    it "evaltest testAdd01" $ do
        evaltest testAdd01 `shouldReturn` "42"
    it "evaltest testAdd02" $ do
        evaltest testAdd02 `shouldReturn` "10"
    it "evaltest testSub01" $ do
        evaltest testSub01 `shouldReturn` "-26"
    it "evaltest testSub02" $ do
        evaltest testSub02 `shouldReturn` "-2"


testLit01 :: ModuleInt
testLit01 = ModuleInt [PrintStmt (Constant 34)]

testNeg01 :: ModuleInt
testNeg01 = ModuleInt [PrintStmt (UnaryOp USub (Constant 42))]

testNeg02 :: ModuleInt
testNeg02 = ModuleInt [PrintStmt (BinOp Add (Constant 8) (UnaryOp USub ((BinOp Add (Constant 1) (Constant 2)))))]

testAdd01 :: ModuleInt
testAdd01 = ModuleInt [PrintStmt (BinOp Add (Constant 8) (Constant 34))]

testAdd02 :: ModuleInt
testAdd02 = ModuleInt [PrintStmt (BinOp Add (BinOp Add (Constant 1) (Constant 2)) (BinOp Add (Constant 3) (Constant 4)))]

testSub01 :: ModuleInt
testSub01 = ModuleInt [PrintStmt (BinOp Sub (Constant 8) (Constant 34))]

testSub02 :: ModuleInt
testSub02 = ModuleInt [PrintStmt (BinOp Add (BinOp Sub (Constant 1) (Constant 2)) (BinOp Sub (Constant 3) (Constant 4)))]



