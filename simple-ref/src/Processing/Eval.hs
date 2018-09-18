module Processing.Eval (
    eval
) where

import Data.List
import Data.Bifunctor

import Data.Terms
import Data.Terms.Utils

import Debug.Trace

type Heap = [Term]

tt :: Show a => String -> a -> a
tt msg x = trace (msg ++ ": " ++ (show x)) x
-- tt _ x = x

tsid :: Show a => Int -> a -> a
tsid n a = trace ((concat $ map (const "  ") [1..n]) ++ (show a)) a
-- tsid _ a = a

eval :: EqnContext -> Term -> Term
eval eqns t = evalLoop [] t
    where evalLoop h t = case eval1 0 eqns h t of
                            Just (t', h') -> evalLoop h' t'
                            Nothing -> t

matchContext :: MatchPattern -> Term -> [(VarName, Term)]
matchContext (MatchVar s) t = [(s, t)]
matchContext (MatchRecord ps) (TermRecord _ ts) =
    case sequence $ getRecordType ps ts of
        Nothing -> error "Invalid Match!"
        (Just mp) -> concat $ map (uncurry matchContext) mp
matchContext _ _ = error "Invalid Match!"

numberContext :: [(VarName, Term)] -> [(Int, Term)]
numberContext xs = zipWith (\n p -> first (const n) p) [0..] xs

getMatchContext :: MatchPattern -> Term -> [(Int, Term)]
getMatchContext p t = numberContext $ matchContext p t

letSub :: MatchPattern -> Term -> Term -> Term
letSub p v1 t2 = let
    ctx = tt "letSub ctx" $ getMatchContext p v1
    lc  = length ctx
    ctx' = tt "letSub ctx'" $ map (second (termShift lc)) ctx
    in walk ctx' t2
    where
        walk [] t = t
        walk ((j, s):cs) t = walk cs $ termSub j s t

eval1 :: Int -> EqnContext -> Heap -> Term -> Maybe (Term, Heap)
eval1 level eqns heap term
    | (TermApp i t1 t2) <- term
    , (Just (t1', h')) <- eval1 (level+1) eqns heap t1
    = tsid level $ Just (TermApp i t1' t2, h')

    | (TermApp i v1 t2) <- term
    , isValue v1
    , (Just (t2', h')) <- eval1 (level+1) eqns heap t2
    = tsid level $ Just (TermApp i v1 t2', h')

    | (TermApp _ (TermAbs i n _ t1) t2) <- term
    , isValue t2
    = tsid level $ Just (termShift (-1) $ (termSub 0 t2 t1), heap)

    | (TermIf i t1 t2 t3) <- term
    , isValue t1
    , isValue t2
    , (Just (t3', h')) <- eval1 (level+1) eqns heap t3
    = tsid level $ Just (TermIf i t1 t2 t3', h')

    | (TermIf i t1 t2 t3) <- term
    , isValue t1
    , (Just (t2', h')) <- eval1 (level+1) eqns heap t2
    = tsid level $ Just (TermIf i t1 t2' t3, h')

    | (TermIf i t1 t2 t3) <- term
    , (Just (t1', h')) <- eval1 (level+1) eqns heap t1
    = tsid level $ Just (TermIf i t1' t2 t3, h')

    | (TermIf _ (TermTrue _) t2 _) <- term
    = tsid level $ Just (t2, heap)

    | (TermIf _ (TermFalse _) _ t3) <- term
    = tsid level $ Just (t3, heap)

    | (TermSucc i t1) <- term
    , (Just (t1', h')) <- eval1 (level+1) eqns heap t1
    = tsid level $ Just (TermSucc i t1', h')

    | (TermSucc _ v1) <- term
    , (TermNat i n) <- v1
    = tsid level $ Just (TermNat i (n + 1), heap)

    | (TermPred i (TermNat _ 0)) <- term
    = tsid level $ Just (TermNat i 0, heap)

    | (TermPred i t1) <- term
    , (Just (t1', h')) <- eval1 (level+1) eqns heap t1
    = tsid level $ Just (TermPred i t1', h')

    | (TermPred _ v1) <- term
    , (TermNat i n) <- v1
    = tsid level $ Just (TermNat i (n - 1), heap)

    | (TermIsZero i t1) <- term
    , (Just (t1', h')) <- eval1 (level+1) eqns heap t1
    = tsid level $ Just (TermIsZero i t1', h')

    | (TermIsZero i (TermNat _ 0)) <- term
    = tsid level $ Just (TermTrue i, heap)

    | (TermIsZero i (TermNat _ _)) <- term
    = tsid level $ Just (TermFalse i, heap)

    | (TermTup i ts) <- term
    , (values, (nv:nvs)) <- partition (isValue) ts
    , (Just (nv', h')) <- eval1 (level+1) eqns heap nv
    = tsid level $ Just (TermTup i (values ++ (nv':nvs)), h')

    | (TermTupProjection i v1 n) <- term
    , isValue v1
    , (TermTup _ ts) <- v1
    = tsid level $ Just (ts !! (fromIntegral n), heap)

    | (TermTupProjection i t1 n) <- term
    , (Just (t1', h')) <- eval1 (level+1) eqns heap t1
    = tsid level $ Just (TermTupProjection i t1' n, h')

    | (TermRecord i ts) <- term
    , (values, ((l,nv):nvs)) <- partition (isValue.snd) ts
    , (Just (nv', h')) <- eval1 (level+1) eqns heap nv
    = tsid level $ Just (TermRecord i (values ++ (l, nv'):nvs), h')

    | (TermRecordProjection i v1 l) <- term
    , isValue v1
    , (TermRecord _ ts) <- v1
    = tsid level $ (\x -> (x, heap)) <$> lookup l ts

    | (TermRecordProjection i t1 l) <- term
    , (Just (t1', h')) <- eval1 (level+1) eqns heap t1
    = tsid level $ Just (TermRecordProjection i t1' l, h')

    | (TermTag i s t1 tt) <- term
    , (Just (t1', h')) <- eval1 (level+1) eqns heap t1
    = tsid level $ Just (TermTag i s t1' tt, h')

    | (TermCase i (TermTag _ label v1 tt) bindings) <- term
    , isValue v1
    , (Just (_, term)) <- getCaseBindingForLabel bindings label
    = tsid level $ Just (termSub 0 v1 term, heap)

    -- Refs
    | (TermRef i v1) <- term
    , isValue v1
    = let nidx = length heap
          h' = heap ++ [v1]
      in tsid level $ Just (TermLoc nidx, h')

    | (TermRef i t1) <- term
    , (Just (t1', h')) <- eval1 (level+1) eqns heap t1
    = tsid level $ Just (TermRef i t1', h')

    | (TermDeref _ v1) <- term
    , (TermLoc l) <- v1
    , l < length heap
    = tsid level $ Just (heap !! l, heap)

    | (TermDeref i t1) <- term
    , not $ isValue t1
    , (Just (t1', h')) <- eval1 (level+1) eqns heap t1
    = tsid level $ Just (TermDeref i t1', h')

    -- Let is like a binder from left to right, without the actual binding...
    | (TermLet i m v1 t2) <- term
    , isValue v1
    = tsid level $ Just (letSub m v1 t2, heap)

    | (TermLet i m t1 t2) <- term
    , (Just (t1', h')) <- eval1 (level+1) eqns heap t1
    = tsid level $ Just (TermLet i m t1' t2, h')

    | (TermVar _ n) <- term
    , n >= 0 && n < length eqns 
    = tsid level $ Just (indexToEquation eqns n, heap)

    | _ <- term
    = tsid level $ Nothing
