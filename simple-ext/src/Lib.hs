module Lib
(
    Context,
    TypeContext,
    Term,
    PState(..),
    parse,
    tokenize,
    showContext,
    showTermInContext,
    eval,
    typeOf,
    desugarTypes,
    generateContextFromEquations
) where

import Data.Terms
import Processing.Eval
import Processing.Typecheck
import Parse.Parse
import Parse.Tokenize