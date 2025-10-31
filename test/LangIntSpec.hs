module LangIntSpec (spec) where

import Test.Hspec
import SpecPrint (specPrint)
import SpecEval  (specEval)
import SpecPartialEval (specPartialEval)

-- Main module for test driver
spec :: Spec
spec = do
  specPrint
  specEval
  specPartialEval
