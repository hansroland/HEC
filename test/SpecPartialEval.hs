module SpecPartialEval (specPartialEval) where

import Test.Hspec
import LangInt.Syntax
import LangInt.PartialEval

specPartialEval :: Spec
specPartialEval = do
  describe "Tests for module PartialEval" $ do
    it "peModule testPart01" $ do
        (peModule testPart01) `shouldBe` ModuleInt [ExprStmt (Constant 2)]
    it "peModule testPart02" $ do
        (peModule testPart02) `shouldBe` ModuleInt [PrintStmt (Constant 2)]
    it "peModule testPart03" $ do
        (peModule testPart03) `shouldBe`
           ModuleInt [ExprStmt (BinOp Add (Constant 3) (BinOp Add (Constant 3) (Call "getInt" [])))]

-- Parts of the syntax
ex00 :: Expr
ex00 = BinOp Add (BinOp Add (Constant 1) (Constant 2)) (BinOp Sub (Constant 3) (Constant 4))

ex01 :: Expr
ex01 = BinOp Add (BinOp Add (Constant 1) (Constant 2)) (BinOp Add (Constant 3) (Call "getInt" []))


testPart01 :: ModuleInt
testPart01 = ModuleInt [ExprStmt ex00 ]

testPart02 :: ModuleInt
testPart02 = ModuleInt [PrintStmt ex00 ]

testPart03 :: ModuleInt
testPart03 = ModuleInt [ExprStmt ex01 ]


