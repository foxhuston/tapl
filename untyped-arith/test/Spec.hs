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


return []
runTests = $quickCheckAll

main :: IO ()
main = do
    runTests
    return ()