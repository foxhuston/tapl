module Lib
    ( someFunc
    ) where

import Data.Terms
import Processing.Eval

t1 = (TermIf Blank
        (TermFalse Blank)
        (TermSucc Blank
            (TermZero Blank))
        (TermPred Blank
            (TermSucc Blank
                (TermZero Blank))))

someFunc :: IO ()
someFunc = print $ eval t1
