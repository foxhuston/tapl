module Processing.Eval (
    eval
) where

import Data.Terms

eval :: Term -> Term
eval t =
    case eval1 t of
        Just t' -> eval t'
        Nothing -> t
        

eval1 :: Term -> Maybe Term
eval1 term
    | (TermIf _ (TermTrue _) t2 _) <- term
    = Just t2

    | (TermIf _ (TermFalse _) _ t3) <- term
    = Just t3

    | (TermIf info t1 t2 t3) <- term
    , (Just t12) <- eval1 t1
    = Just $ TermIf info t12 t2 t3

    | (TermSucc info t1) <- term
    , (Just t12) <- eval1 t1
    = Just $ TermSucc info t12

    | (TermPred _ (TermZero zinfo)) <- term
    = Just $ TermZero zinfo

    | (TermPred _ (TermSucc _ nv)) <- term
    , isNumericValue nv
    = Just nv

    | (TermPred pinfo t1) <- term
    , (Just t12) <- eval1 t1
    = Just $ TermPred pinfo t12

    | (TermIsZero _ (TermZero _)) <- term
    = Just $ TermTrue Blank

    | (TermIsZero _ (TermSucc _ nv1)) <- term
    , isNumericValue nv1
    = Just $ TermFalse Blank

    | (TermIsZero zinfo t1) <- term
    , (Just t12) <- eval1 t1
    = Just $ TermIsZero zinfo t12

    | _ <- term
    = Nothing