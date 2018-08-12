{-# Language TemplateHaskell #-}

module Unit (
    unitTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Parse
import Data.Terms

case_parseZero =
    case parseUntypedArith "0" of
        Right [(TermZero _)] -> return ()
        otherwise -> assertFailure "Expected (TermZero _)"

case_parseTrue =
    case parseUntypedArith "true" of
        Right [(TermTrue _)] -> return ()
        otherwise -> assertFailure "Expected (TermTrue _)"

case_parseFalse =
    case parseUntypedArith "false" of
        Right [(TermFalse _)] -> return ()
        otherwise -> assertFailure "Expected (TermFalse _)"

case_parseIsZero = 
    case parseUntypedArith "iszero 0" of
        Right [(TermIsZero _ (TermZero _))] -> return ()
        otherwise -> assertFailure "Expected (TermIsZero _ (TermZero _))"

case_parseSuccZero =
    case parseUntypedArith "succ 0" of
        Right [(TermSucc _ (TermZero _))] -> return ()
        otherwise -> assertFailure "Expected (TermSucc _ (TermZero _))"

case_parseSuccSuccZero =
    case parseUntypedArith "succ succ 0" of
        Right [(TermSucc _ (TermSucc _ (TermZero _)))] -> return ()
        otherwise -> assertFailure "Expected (TermSucc _ (TermSucc _ (TermZero _)))"

case_parseSuccSuccZeroParens =
    case parseUntypedArith "(succ (succ 0))" of
        Right [(TermSucc _ (TermSucc _ (TermZero _)))] -> return ()
        otherwise -> assertFailure "Expected (TermSucc _ (TermSucc _ (TermZero _)))"


case_parsePredZero =
    case parseUntypedArith "pred 0" of
        Right [(TermPred _ (TermZero _))] -> return ()
        otherwise -> assertFailure "Expected (TermSucc _ (TermZero _))"

case_parsePredPredZero =
    case parseUntypedArith "pred pred 0" of
        Right [(TermPred _ (TermPred _ (TermZero _)))] -> return ()
        otherwise -> assertFailure "Expected (TermSucc _ (TermSucc _ (TermZero _)))"

case_parsePredSuccZero =
    case parseUntypedArith "pred succ 0" of
        Right [(TermPred _ (TermSucc _ (TermZero _)))] -> return ()
        otherwise -> assertFailure "Expected (TermSucc _ (TermSucc _ (TermZero _)))"

case_parseIfThenElse =
    case parseUntypedArith "if (iszero 0) then true else false" of
        Right [(TermIf _ (TermIsZero _ (TermZero _)) (TermTrue _) (TermFalse _))] -> return ()
        otherwise -> assertFailure "Expected (TermIf _ (TermIsZero _ (TermZero _)) (TermTrue _) (TermFalse _))"

case_MultipleTermProgram =
    case parseUntypedArith "if (iszero 0) then true else false\n\nsucc 0" of
        Right([(TermIf _ (TermIsZero _ (TermZero _)) (TermTrue _) (TermFalse _)), (TermSucc _ (TermZero _))]) -> return ()
        otherwise -> assertFailure "Expected [(TermIf _ (TermIsZero _ (TermZero _)) (TermTrue _) (TermFalse _)), (TermSucc _ (TermZero _))]"

unitTests :: TestTree
unitTests = $(testGroupGenerator)