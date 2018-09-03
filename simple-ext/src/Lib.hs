module Lib
(
    Context,
    Term,
    PState(..),
    parse,
    tokenize,
    showTermInContext,
    eval,
    typeof,
    generateContextFromEquations
) where

import Data.Terms
import Processing.Eval
import Processing.Typecheck
import Parse.Parse
import Parse.Tokenize