{-# Language TemplateHaskell #-}

module Unit (
    unitTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Parse
import Data.Terms

case_ItIsLeftAssociative =
    let (Right (_, t)) = parseUntypedLambda "x y z" in
        t @?= TermApp Blank (
            TermApp Blank
                (TermVar Blank 0)
                (TermVar Blank 1)
            )
            (TermVar Blank 2)

case_ItParsesAbstractions =
    let (Right (_, t)) = parseUntypedLambda "\\x. x" in
        t @?= TermAbs Blank "x" (TermVar Blank 0)

case_NestedParseWorks =
    let (Right (_, t)) = parseUntypedLambda "(\\x. x x) (\\x . x x)" in
        t @?= TermApp Blank
                (TermAbs Blank "x" (TermApp Blank (TermVar Blank 0) (TermVar Blank 0)))
                (TermAbs Blank "x" (TermApp Blank (TermVar Blank 0) (TermVar Blank 0)))

unitTests :: TestTree
unitTests = $(testGroupGenerator)