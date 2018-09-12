module Lib
(
    Context,
    EqnContext,
    PState(..),
    Term,
    TypeContext,
    desugarTerm,
    desugarTypes,
    eval,
    generateContextFromEquations,
    parse,
    showContext,
    showTermInContext,
    tokenize,
    typeOf
) where

import Data.Terms
import Processing.Eval
import Processing.Typecheck
import Parse.Parse
import Parse.Tokenize
