module Processing.Eval (
    eval
) where

import Data.Terms

eval :: Term -> Term
eval t =
    case eval1 t of
        Just t' -> eval t'
        Nothing -> t
        

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
    where
        walk c (TermVar i k)       = TermVar i $ (if k < c then k else k + d)
        walk c (TermAbs i h ty t1) = TermAbs i h ty $ walk (c + 1) t1
        walk c (TermApp i t1 t2)   = TermApp i (walk c t1) (walk c t2)
        walk c (TermIf i t1 t2 t3) = TermIf i (walk c t1) (walk c t2) (walk c t3)
        walk _ (TermTrue i)        = TermTrue i
        walk _ (TermFalse i)       = TermFalse i

termSub :: Int -> Term -> Term -> Term
termSub j s t1@(TermVar _ k)    = if j == k then s else t1
termSub j s (TermAbs i h ty t1) = TermAbs i h ty $ termSub (j + 1) (termShift 1 s) t1
termSub j s (TermApp i t1 t2)   = TermApp i (termSub j s t1) (termSub j s t2)
termSub j s (TermIf i t1 t2 t3) = TermIf i (termSub j s t1) (termSub j s t2) (termSub j s t3)
termSub _ _ (TermTrue i)        = TermTrue i
termSub _ _ (TermFalse i)       = TermFalse i

eval1 :: Term -> Maybe Term
eval1 term
    | (TermApp i t1 t2) <- term
    , (Just t1') <- eval1 t1
    = Just $ TermApp i t1' t2

    | (TermApp i t1 t2) <- term
    , isValue t1
    , (Just t2') <- eval1 t2
    = Just $ TermApp i t1 t2'

    | (TermApp _ (TermAbs i n _ t1) t2) <- term
    , isValue t2
    = Just $ termShift (-1) (termSub 0 t2 t1)

    | (TermIf i t1 t2 t3) <- term
    , isValue t1
    , isValue t2
    , (Just t3') <- eval1 t3
    = Just $ TermIf i t1 t2 t3'

    | (TermIf i t1 t2 t3) <- term
    , isValue t1
    , (Just t2') <- eval1 t2
    = Just $ TermIf i t1 t2' t3

    | (TermIf i t1 t2 t3) <- term
    , (Just t1') <- eval1 t1
    = Just $ TermIf i t1' t2 t3

    | (TermIf _ (TermTrue _) t2 _) <- term
    = Just t2

    | (TermIf _ (TermFalse _) _ t3) <- term
    = Just t3


    | _ <- term
    = Nothing