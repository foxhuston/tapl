{-# Language TemplateHaskell #-}

module Unit (
    unitTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

case_ListAreEq = [1, 2] `compare` [1, 2] @?= EQ

unitTests :: TestTree
unitTests = $(testGroupGenerator)