module LangSrc.LangSrcSpec (spec) where

import Test.Hspec
import LangSrc.SpecPrint (specPrint)
import LangSrc.SpecEval  (specEval)
import LangSrc.SpecPartialEval (specPartialEval)

-- Main module for test driver
spec :: Spec
spec = do
  specPrint
  specEval
  specPartialEval
