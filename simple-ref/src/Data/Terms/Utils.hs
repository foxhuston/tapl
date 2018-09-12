module Data.Terms.Utils (
    termShift,
    termSub
)
where

import Data.Bifunctor
import Data.Terms.Terms

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
    where
        walk c (TermVar i k)                 = TermVar i $ (if k < c then k else k + d)
        walk c (TermAbs i h ty t1)           = TermAbs i h ty $ walk (c + 1) t1
        walk c (TermLet i p t1 t2)           = TermLet i p (walk c t1) (walk (c+1) t2)
        walk c (TermApp i t1 t2)             = TermApp i (walk c t1) (walk c t2)
        walk c (TermIf i t1 t2 t3)           = TermIf i (walk c t1) (walk c t2) (walk c t3)
        walk c (TermIsZero i t1)             = TermIsZero i (walk c t1)
        walk c (TermSucc i t1)               = TermSucc i (walk c t1)
        walk c (TermPred i t1)               = TermPred i (walk c t1)
        walk c (TermTup i ts)                = TermTup i (map (walk c) ts)
        walk c (TermTupProjection i t1 n)    = TermTupProjection i (walk c t1) n
        walk c (TermRecord i ts)             = TermRecord i (map (second $ walk c) ts)
        walk c (TermRecordProjection i t1 l) = TermRecordProjection i (walk c t1) l
        walk c (TermTag i l t1 tt)           = TermTag i l (walk c t1) tt
        walk c (TermCase i t1 ts)            = TermCase i (walk c t1) (map (second $ walk (c+1)) ts)
        walk _ (TermTrue i)                  = TermTrue i
        walk _ (TermFalse i)                 = TermFalse i
        walk _ (TermString i s)              = TermString i s
        walk _ t@(TermNat _ _)               = t
        walk _ t                             = error ("Attempt to shift " ++ (show t))

termSub :: Int -> Term -> Term -> Term
termSub j s t1@(TermVar _ k)              = if j == k then s else t1
termSub j s (TermAbs i h ty t1)           = TermAbs i h ty $ termSub (j + 1) (termShift 1 s) t1
termSub j s (TermApp i t1 t2)             = TermApp i (termSub j s t1) (termSub j s t2)
termSub j s (TermLet i p t1 t2)           = TermLet i p (termSub j s t1) (termSub (j+1) s t2)
termSub j s (TermIf i t1 t2 t3)           = TermIf i (termSub j s t1) (termSub j s t2) (termSub j s t3)
termSub j s (TermIsZero i t1)             = TermIsZero i (termSub j s t1)
termSub j s (TermSucc i t1)               = TermSucc i (termSub j s t1)
termSub j s (TermPred i t1)               = TermPred i (termSub j s t1)
termSub j s (TermTup i ts)                = TermTup i (map (termSub j s) ts)
termSub j s (TermTupProjection i t1 n)    = TermTupProjection i (termSub j s t1) n
termSub j s (TermRecord i ts)             = TermRecord i (map (second $ termSub j s) ts)
termSub j s (TermRecordProjection i t1 l) = TermRecordProjection i (termSub j s t1) l
termSub j s (TermTag i l t1 tt)           = TermTag i l (termSub j s t1) tt
termSub j s (TermCase i t1 ts)            = TermCase i (termSub j s t1) (map (second $ termSub (j+1) s) ts)
termSub _ _ (TermString i s)              = TermString i s
termSub _ _ (TermTrue i)                  = TermTrue i
termSub _ _ (TermFalse i)                 = TermFalse i
termSub _ _ t@(TermNat _ _)               = t
