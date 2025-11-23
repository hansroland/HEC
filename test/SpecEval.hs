{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}

module SpecEval (specEval) where

import Test.Hspec
import LangInt.Syntax
import LangInt.Eval

import System.IO.Silently

-- This is very hacky! But we get always 2 lines!!!
-- I don't know yet why !!!
-- Run the test in the IO Monad
evaltest :: IO () -> IO String
evaltest e = do
    output <- capture_ e
    pure $ checkdata $ lines output
  where
    checkdata :: [String] -> String
    checkdata (l : _)  = l
    checkdata []       = "No output on stdout found"

specEval :: Spec
specEval = do
  describe "Tests for module Eval" $ do
    it "evaltest testLit01" $ do
        evaltest (testLit01 @(IO Int)) `shouldReturn` "34"
    it "evaltest testNeg01" $ do
        evaltest (testNeg01 @(IO Int)) `shouldReturn` "-42"
    it "evaltest testNeg02" $ do
        evaltest (testNeg02 @(IO Int)) `shouldReturn` "5"
    it "evaltest testAdd01" $ do
        evaltest (testAdd01 @(IO Int)) `shouldReturn` "42"
    it "evaltest testAdd02" $ do
        evaltest (testAdd02 @(IO Int)) `shouldReturn` "10"
    it "evaltest testSub01" $ do
        evaltest (testSub01 @(IO Int)) `shouldReturn` "-26"
    it "evaltest testSub02" $ do
        evaltest (testSub02 @(IO Int)) `shouldReturn` "-2"

testLit01 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testLit01 = qprint @f (int @f 34)

testNeg01 ::forall f. (Expr f, Stmt f) => StmtTy f ()
testNeg01 = qprint @f (usub @f (int @f 42))

testNeg02 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testNeg02 = qprint @f (add @f (int @f 8) (usub @f ((add @f (int @f 1) (int @f 2)))))

testAdd01 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testAdd01 = qprint @f (add @f (int @f 8) (int @f 34))

testAdd02 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testAdd02 = qprint @f  (add @f (add @f (int @f 1) (int @f 2)) (add @f (int @f 3) (int @f 4)))

testSub01 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testSub01 = qprint @f  (sub @f (int @f 8) (int @f 34))

testSub02 :: forall f. (Expr f, Stmt f) => StmtTy f ()
testSub02 = qprint @f (add @f (sub @f (int @f 1) (int @f 2)) (sub @f (int @f 3) (int @f 4)))
