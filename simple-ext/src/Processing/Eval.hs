module Processing.Eval (
    eval
) where

import Data.List
import Data.Bifunctor

import Data.Terms

eval :: Term -> Term
eval t =
    case eval1 t of
        Just t' -> eval t'
        Nothing -> t

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
    where
        walk c (TermVar i k)                 = TermVar i $ (if k < c then k else k + d)
        walk c (TermAbs i h ty t1)           = TermAbs i h ty $ walk (c + 1) t1
        walk c (TermLet i p t1 t2)           = TermLet i p (walk c t1) (walk c t2)
        walk c (TermApp i t1 t2)             = TermApp i (walk c t1) (walk c t2)
        walk c (TermIf i t1 t2 t3)           = TermIf i (walk c t1) (walk c t2) (walk c t3)
        walk c (TermIsZero i t1)             = TermIsZero i (walk c t1)
        walk c (TermSucc i t1)               = TermSucc i (walk c t1)
        walk c (TermPred i t1)               = TermPred i (walk c t1)
        walk c (TermTup i ts)                = TermTup i (map (walk c) ts)
        walk c (TermTupProjection i t1 n)    = TermTupProjection i (walk c t1) n
        walk c (TermRecord i ts)             = TermRecord i (map (second $ walk c) ts)
        walk c (TermRecordProjection i t1 l) = TermRecordProjection i (walk c t1) l
        walk _ (TermTrue i)                  = TermTrue i
        walk _ (TermFalse i)                 = TermFalse i
        walk _ t@(TermNat _ _)               = t

termSub :: Int -> Term -> Term -> Term
termSub j s t1@(TermVar _ k)              = if j == k then s else t1
termSub j s (TermAbs i h ty t1)           = TermAbs i h ty $ termSub (j + 1) (termShift 1 s) t1
termSub j s (TermApp i t1 t2)             = TermApp i (termSub j s t1) (termSub j s t2)
termSub j s (TermLet i p t1 t2)           = TermLet i p (termSub j s t1) (termSub (j + 1) (termShift 1 s) t2)
termSub j s (TermIf i t1 t2 t3)           = TermIf i (termSub j s t1) (termSub j s t2) (termSub j s t3)
termSub j s (TermIsZero i t1)             = TermIsZero i (termSub j s t1)
termSub j s (TermSucc i t1)               = TermSucc i (termSub j s t1)
termSub j s (TermPred i t1)               = TermPred i (termSub j s t1)
termSub j s (TermTup i ts)                = TermTup i (map (termSub j s) ts)
termSub j s (TermTupProjection i t1 n)    = TermTupProjection i (termSub j s t1) n
termSub j s (TermRecord i ts)             = TermRecord i (map (second $ termSub j s) ts)
termSub j s (TermRecordProjection i t1 l) = TermRecordProjection i (termSub j s t1) l
termSub _ _ (TermTrue i)                  = TermTrue i
termSub _ _ (TermFalse i)                 = TermFalse i
termSub _ _ t@(TermNat _ _)               = t


matchContext :: MatchPattern -> Term -> [(String, Term)]
matchContext (MatchVar s) t = [(s, t)]
matchContext (MatchRecord ps) (TermRecord _ ts) =
    case sequence $ getRecordType ps ts of
        Nothing -> error "Invalid Match!"
        (Just mp) -> concat $ map (uncurry matchContext) mp
matchContext _ _ = error "Invalid Match!"

numberContext :: [(String, Term)] -> [(Int, Term)]
numberContext xs = zipWith (\n p -> first (const n) p) [0..] xs

getMatchContext :: MatchPattern -> Term -> [(Int, Term)]
getMatchContext p t = numberContext $ matchContext p t

letSub :: MatchPattern -> Term -> Term -> Term
letSub p v1 t2 = let
    ctx = getMatchContext p v1
    lc  = length ctx
    ctx' = map (second (termShift lc)) ctx
    in walk ctx' t2
    where
        walk [] t = t
        walk ((j, s):cs) t = walk cs $ termSub j s t

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

    | (TermSucc i t1) <- term
    , (Just t1') <- eval1 t1
    = Just $ TermSucc i t1'

    | (TermSucc _ v1) <- term
    , (TermNat i n) <- v1
    = Just $ TermNat i (n + 1)

    | (TermPred i (TermNat _ 0)) <- term
    = Just $ TermNat i 0

    | (TermPred i t1) <- term
    , (Just t1') <- eval1 t1
    = Just $ TermPred i t1'

    | (TermPred _ v1) <- term
    , (TermNat i n) <- v1
    = Just $ TermNat i (n - 1)

    | (TermIsZero i t1) <- term
    , (Just t1') <- eval1 t1
    = Just $ TermIsZero i t1'

    | (TermIsZero i (TermNat _ 0)) <- term
    = Just $ TermTrue i

    | (TermIsZero i (TermNat _ _)) <- term
    = Just $ TermFalse i

    | (TermTup i ts) <- term
    , (values, (nv:nvs)) <- partition (isValue) ts
    , (Just nv') <- eval1 nv
    = Just $ TermTup i (values ++ (nv':nvs))

    | (TermTupProjection i v1 n) <- term
    , isValue v1
    , (TermTup _ ts) <- v1
    = Just $ ts !! (fromIntegral n)

    | (TermTupProjection i t1 n) <- term
    , (Just t1') <- eval1 t1
    = Just $ TermTupProjection i t1' n

    | (TermRecord i ts) <- term
    , (values, ((l,nv):nvs)) <- partition (isValue.snd) ts
    , (Just nv') <- eval1 nv
    = Just $ TermRecord i (values ++ (l, nv'):nvs)

    | (TermRecordProjection i v1 l) <- term
    , isValue v1
    , (TermRecord _ ts) <- v1
    = lookup l ts

    | (TermRecordProjection i t1 l) <- term
    , (Just t1') <- eval1 t1
    = Just $ TermRecordProjection i t1' l

    -- Let is like a binder from left to right, without the actual binding...
    | (TermLet i m v1 t2) <- term
    , isValue v1
    = Just $ letSub m v1 t2

    | (TermLet i m t1 t2) <- term
    , (Just t1') <- eval1 t1
    = Just $ TermLet i m t1' t2

    | _ <- term
    = Nothing