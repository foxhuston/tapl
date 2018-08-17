module Lib
(
    Context,
    Term,
    parseUntypedLambda,
    showTermInContext,
    eval
) where

import Data.Terms
import Processing.Eval
import Parse