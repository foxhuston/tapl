module Lib
(
    Context,
    EqnContext,
    Heap,
    PState(..),
    Term,
    TypeContext,
    desugarTerm,
    desugarTypes,
    eval,
    generateContextFromEquations,
    generateHeapFromContext,
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
