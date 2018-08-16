{-# Language TemplateHaskell #-}

module Unit (
    unitTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

-- import Parse
import Data.Terms

case_itApplies =
    (TermAbs Blank "x" (TermVar Blank ))


unitTests :: TestTree
unitTests = $(testGroupGenerator)