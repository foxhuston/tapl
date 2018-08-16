{-# Language TemplateHaskell #-}

module Quick (
    quickTests
) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Data.Terms
-- import Helpers.Terms

-- prop_TermIsNumericValueReturnsTrue :: NumericTerm -> Bool
-- prop_TermIsNumericValueReturnsTrue (NumericTerm term) =
--   isNumericValue term

-- prop_TermIsNotNumericValueReturnsFalse :: BoolTerm -> Bool
-- prop_TermIsNotNumericValueReturnsFalse (BoolTerm term) =
--   not $ isNumericValue term

-- prop_IsValueTermReturnsTrue :: ValueTerm -> Bool
-- prop_IsValueTermReturnsTrue (ValueTerm term) =
--     isValue term

-- prop_IsValueTermReturnsFalse :: NonValueTerm -> Bool
-- prop_IsValueTermReturnsFalse (NonValueTerm term) =
--     not $ isValue term

-- quickTests :: TestTree
-- quickTests = $(testGroupGenerator)