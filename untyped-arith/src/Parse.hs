module Parse (
    parseUntypedArith
) where

{-
Example Programs:

if true then (succ 0) else 0

if (isZero succ 0) then 0 else succ 0

succ succ succ 0

pred succ 0

-}


import Text.Parsec

parseUntypedArith = ()