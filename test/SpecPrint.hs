module SpecPrint (specPrint) where

import Test.Hspec
import LangInt.Syntax
import LangInt.Print

-- Tests for the Print interpreters
specPrint :: Spec
specPrint = do
  describe "Tests for module Print - quickprint" $ do
    it "qprint evalLit01" $ do
        qprint testLit01 `shouldBe` "qprint 34\n"
    it "qprint testNeg01" $ do
        qprint testNeg01 `shouldBe` "qprint -42\n"
    it "qprint testNeg02" $ do
        qprint testNeg02 `shouldBe` "qprint (8 + -(1 + 2))\n"
    it "qprint testAdd01" $ do
        qprint testAdd01 `shouldBe` "qprint (8 + 34)\n"
    it "qprint testAdd02" $ do
        qprint testAdd02 `shouldBe` "qprint ((1 + 2) + (3 + 4))\n"
    it "qprint testSub01" $ do
        qprint testSub01 `shouldBe` "qprint (8 - 34)\n"
    it "qprint testSub02" $ do
        qprint testSub02 `shouldBe` "qprint ((1 - 2) - (3 - 4))\n"

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
testSub02 = ModuleInt [PrintStmt (BinOp Sub (BinOp Sub (Constant 1) (Constant 2)) (BinOp Sub (Constant 3) (Constant 4)))]
