{-# Language TemplateHaskell #-}

import Test.QuickCheck
import Data.Terms
import Helpers.Terms

prop_TermIsNumericValueReturnsTrue :: NumericTerm -> Bool
prop_TermIsNumericValueReturnsTrue (NumericTerm term) =
  isNumericValue term

prop_TermIsNotNumericValueReturnsFalse :: BoolTerm -> Bool
prop_TermIsNotNumericValueReturnsFalse (BoolTerm term) =
  not $ isNumericValue term

prop_IsValueTermReturnsTrue :: ValueTerm -> Bool
prop_IsValueTermReturnsTrue (ValueTerm term) =
    isValue term

prop_IsValueTermReturnsFalse :: NonValueTerm -> Bool
prop_IsValueTermReturnsFalse (NonValueTerm term) =
    not $ isValue term

return []
runTests = $quickCheckAll

main :: IO ()
main = do
    runTests
    return ()