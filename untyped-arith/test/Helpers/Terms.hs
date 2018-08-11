module Helpers.Terms (
    NumericTerm(..),
    BoolTerm(..)
) where

import Test.QuickCheck
import Data.Terms

instance Arbitrary Term where
    arbitrary = sized arbitrarySizedTerm

arbitrarySizedTerm :: Int -> Gen Term
arbitrarySizedTerm n = oneof [arbitrarySizedIfTerm n, arbitrarySizedUnaryTerm n]

arbitrarySizedIfTerm :: Int -> Gen Term
arbitrarySizedIfTerm 0 = arbitrarySizedUnaryTerm 0
arbitrarySizedIfTerm n = do
    a <- choose (0, n - 1)
    b <- choose (0, n - 1)
    [a, b, c] <- shuffle [a, b, n - 1]
    t1 <- arbitrarySizedTerm a
    t2 <- arbitrarySizedTerm b
    t3 <- arbitrarySizedTerm c
    return $ TermIf Blank t1 t2 t3

arbitrarySizedUnaryTerm :: Int -> Gen Term
arbitrarySizedUnaryTerm 0 = elements [TermTrue Blank, TermFalse Blank, TermZero Blank]
arbitrarySizedUnaryTerm n = do
    unary <- elements [TermIsZero Blank, TermSucc Blank, TermPred Blank]
    subterm <- arbitrarySizedTerm (n - 1)
    return $ unary subterm

newtype NumericTerm = NumericTerm Term
    deriving (Show)

instance Arbitrary NumericTerm where
    arbitrary = sized arbitrarySizedNumericTerm

arbitrarySizedNumericTerm :: Int -> Gen NumericTerm
arbitrarySizedNumericTerm 0 = return $ NumericTerm $ TermZero Blank
arbitrarySizedNumericTerm n = do
    unary <- elements [TermSucc Blank, TermPred Blank]
    (NumericTerm subterm) <- arbitrarySizedNumericTerm (n - 1)
    return $ NumericTerm $ unary subterm

newtype BoolTerm = BoolTerm Term
    deriving (Show)

instance Arbitrary BoolTerm where
    arbitrary = do
        term <- elements [TermTrue Blank, TermFalse Blank]
        return $ BoolTerm term