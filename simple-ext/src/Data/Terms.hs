module Data.Terms (
    Binding(..),
    CaseTag(..),
    Context(..),
    EqnContext(..),
    Info(..),
    MatchPattern(..),
    Term(..),
    TermType (..),
    TypeContext(..),
    VarName(..),

    addBinding,
    desugarTerm,
    getCaseBindingForLabel,
    getIndexFromContext,
    getNameForType,
    getRecordType,
    getTypeForName,
    getTypeForVariantLabel,
    getTypeFromContext,
    indexToEquation,
    isValue,
    mapTerm,
    showContext,
    showTermInContext
) where

import Data.List (findIndex, intercalate, foldl', find)
import Data.Bifunctor (first, second)
import Data.Maybe (isJust, fromJust, fromMaybe)

import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (try)

import Data.Terms.Terms
import Data.Terms.Utils

import Debug.Trace

-- (!?) :: [a] -> Int -> Maybe a
-- (!?) l n = case unsafePerformIO $ try (return $ l !! n) of
--     Right a -> Just a
--     Left _ -> Nothing

tt :: Show a => String -> a -> a
tt msg a = trace (msg ++ ": " ++ show a) a

getTypeForName :: TypeContext -> TermType -> TermType
getTypeForName tc t
    | (TypeUser name) <- t
    , (Just b) <- lookup name tc
    = getTypeForName tc b

    | (TypeTuple ts) <- t
    = TypeTuple $ map (getTypeForName tc) ts

    | (TypeRecord ts) <- t
    = TypeRecord $ map (second (getTypeForName tc)) ts

    | (TypeArrow t1 t2) <- t
    = TypeArrow (getTypeForName tc t1) (getTypeForName tc t2)

    | (TypeVariant ts) <- t
    = TypeVariant $ map (second (getTypeForName tc)) ts

    | otherwise = t

getUserType :: TypeContext -> TermType -> (Maybe TermType)
getUserType tc t = fmap fst $ find (\(_, b) -> b == t) $ map (first TypeUser) tc

getUserTypeOrType :: TypeContext -> TermType -> TermType
getUserTypeOrType tc t = fromMaybe t $ getUserType tc t

getNameForType :: TypeContext -> TermType -> TermType
getNameForType tc t
    | (TypeTuple ts) <- t
    = getUserTypeOrType tc $ TypeTuple $ map (getNameForType tc) ts

    | (TypeRecord ts) <- t
    = getUserTypeOrType tc $ TypeRecord $ map (second (getNameForType tc)) ts

    | (TypeArrow t1 t2) <- t
    = getUserTypeOrType tc $ TypeArrow (getNameForType tc t1) (getNameForType tc t2)

    | (TypeVariant ts) <- t
    = getUserTypeOrType tc $ TypeVariant $ map (second (getNameForType tc)) ts

    | otherwise
    = getUserTypeOrType tc t


indexToEquation :: EqnContext -> Int -> Term
indexToEquation ctx n = snd $ ctx !! ((length ctx) - 1 - n)

desugarTerm :: Term -> Term
desugarTerm = mapTerm ds
    where
        ds (TermSequence i t1 t2) = let
            t1' = desugarTerm t1
            t2' = desugarTerm t2
            in TermApp i (TermAbs i WildCard (TypeTuple []) (termShift 1 t2')) t1'
        ds t = t

mapTerm :: (Term -> Term) -> Term -> Term
mapTerm f term
    | (TermIf i t1 t2 t3) <- term
    = f (TermIf i (mapTerm f t1) (mapTerm f t2) (mapTerm f t3))

    | (TermLet i p t1 t2) <- term
    = f (TermLet i p (mapTerm f t1) (mapTerm f t2))

    | (TermAbs i name ty t1) <- term
    = f (TermAbs i name ty (mapTerm f (t1)))

    | (TermApp i t1 t2) <- term
    = f (TermApp i (mapTerm f t1) (mapTerm f t2))

    | (TermSucc i t1) <- term
    = f (TermSucc i (mapTerm f t1))

    | (TermPred i t1) <- term
    = f (TermPred i (mapTerm f t1))

    | (TermIsZero i t1) <- term
    = f (TermIsZero i (mapTerm f t1))

    | (TermTup i ts) <- term
    = f (TermTup i (map (mapTerm f) ts))

    | (TermTupProjection i t1 n) <- term
    = f (TermTupProjection i (mapTerm f t1) n)

    | (TermRecord i rs) <- term
    = f (TermRecord i (map (second (mapTerm f)) rs))

    | (TermRecordProjection i t1 l) <- term
    = f (TermRecordProjection i (mapTerm f t1) l)

    | t <- term
    = f t

contextLength :: Context -> Int
contextLength = length

indexToName :: Context -> Int -> VarName
indexToName ctx n = fst $ ctx !! ((length ctx) - 1 - n)

getIndexFromContext :: Context -> String -> Maybe Int
getIndexFromContext ctx name = findIndex (\(s, _) -> (VarName name) == s) $ reverse ctx

getTypeFromContext :: Context -> Int -> Maybe TermType
getTypeFromContext ctx x =
    (\(VarBind tt) -> tt) <$> lookup (indexToName ctx x) ctx

addBinding :: Context -> VarName -> Binding -> Context
addBinding ctx s b = ctx ++ [(s, b)]

hasVar :: Context -> String -> Bool
hasVar ctx name = isJust $ getIndexFromContext ctx name

pickFreshName :: Context -> VarName -> (Context, VarName)
pickFreshName ctx x
    | WildCard <- x
    = (ctx ++ [(VarName "_", NameBind)], x)

    | VarName x <- x
    , ctx `hasVar` x
    = pickFreshName ctx (VarName (x ++ "'"))

    | otherwise = (ctx ++ [(x, NameBind)], x)

pickFreshNames :: Context -> [VarName] -> (Context, [VarName])
pickFreshNames ctx xs = foldl' (\(ctx, names) name ->
    let (ctx', name') = pickFreshName ctx name
    in (ctx', name':names)) (ctx, []) xs

getCaseBindingForLabel :: [(CaseTag, Term)] -> String -> Maybe (VarName, Term)
getCaseBindingForLabel bindings label =
    let tt = find ((==label) . caseLabel . fst) bindings
    in (first caseVar) <$> tt

getMatchNames :: MatchPattern -> [VarName]
getMatchNames (MatchVar x) = [x]
getMatchNames (MatchRecord (r:rs)) = getMatchNames (snd r) ++ (concat $ map (getMatchNames . snd) rs)

getRecordType :: [(String, MatchPattern)] -> [(String, a)] -> [Maybe (MatchPattern, a)]
getRecordType [] _ = []
getRecordType ((label, pattern):ms) ts =
    (do
        ty <- (lookup label ts)
        return (pattern, ty)
    ):getRecordType ms ts

getTypeForVariantLabel :: [(String, TermType)] -> String -> TermType
getTypeForVariantLabel variants label = fromJust $ lookup label variants

showContext :: Context -> String
showContext = concat . map printEntry
    where printEntry :: (VarName, Binding) -> String
          printEntry (name, VarBind tt) = show name ++ ": " ++ show tt ++ "\n\n"
          printEntry (name, _) = error "Term " ++ show name ++ " has no type in context!\n"

showRecordInContext :: String -> Context -> [(String, String)] -> String
showRecordInContext sep ctx ts = intercalate ", " (map (\(l, s) -> l ++ sep ++ s) ts)

showTermInContext :: Context -> Term -> String
showTermInContext ctx (TermAbs _ x ty t1) =
    let (ctx', x') = pickFreshName ctx x in
        "(λ" ++ show x' ++ ": " ++ (show ty) ++ ". " ++ showTermInContext ctx' t1 ++ ")"
showTermInContext ctx (TermApp _ t1 t2) =
    "(" ++ showTermInContext ctx t1 ++ " " ++ showTermInContext ctx t2 ++ ")"
showTermInContext ctx (TermVar _ n) = show $ indexToName ctx n
showTermInContext ctx (TermIf _ t1 t2 t3) =
    "if " ++ showTermInContext ctx t1
          ++ " then "
          ++ showTermInContext ctx t2
          ++ " else "
          ++ showTermInContext ctx t3
showTermInContext ctx (TermSucc _ t1) = "(succ " ++ showTermInContext ctx t1 ++ ")"
showTermInContext ctx (TermPred _ t1) = "(pred " ++ showTermInContext ctx t1 ++ ")"
showTermInContext ctx (TermIsZero _ t1) = "(iszero " ++ showTermInContext ctx t1 ++ ")"
showTermInContext ctx (TermTup _ ts) = "(" ++ intercalate ", " (map (showTermInContext ctx) ts) ++ ")"
showTermInContext ctx (TermTupProjection _ t n) = showTermInContext ctx t ++ "." ++ show n
showTermInContext ctx (TermRecord _ ts) = "{" ++ (showRecordInContext "=" ctx $ map (second (showTermInContext ctx)) ts) ++ "}"
showTermInContext ctx (TermTag _ label term termType) = "<" ++ label ++ "=" ++ (showTermInContext ctx term) ++ "> as " ++ (show termType)
showTermInContext ctx (TermRecordProjection _ t l) = showTermInContext ctx t ++ "." ++ l
showTermInContext ctx (TermLet _ m t1 t2) =
    let (ctx', xs') = pickFreshNames ctx $ getMatchNames m
    in "(let " ++ show m ++ " = " ++ showTermInContext ctx t1
       ++ "\nin " ++ showTermInContext ctx' t2 ++ ")"

showTermInContext ctx (TermCase _ t1 bindings) = --"case expr"
    let freshTerms = map (first (\(CaseTag lab v) -> let (ctx', v') = pickFreshName ctx v
                                                     in (ctx', (lab, v)))) bindings
    in "(case " ++ (showTermInContext ctx t1) ++ " of \n    "
        ++ (intercalate "\n  | " $
                map (\((ctx', (l, x')), t) ->
                    "<" ++ l ++ "=" ++ show x' ++ "> => " ++
                    (showTermInContext ctx' t)) freshTerms)


showTermInContext _ (TermNat _ n) = show n
showTermInContext _ (TermString _ s) = show s
showTermInContext _ (TermTrue _)  = "true"
showTermInContext _ (TermFalse _) = "false"

showTermInContext _ t = error $ "Trying to show: " ++ (show t)

isValue :: Term -> Bool
isValue (TermAbs _ _ _ _) = True
isValue (TermTrue _)      = True
isValue (TermFalse _)     = True
isValue (TermNat _ _)     = True
isValue (TermTup _ [])    = True
isValue (TermString _ _)  = True
isValue (TermTup _ ts)    = all isValue ts
isValue (TermRecord _ ts) = all (isValue.snd) ts
isValue (TermTag _ _ t _) = isValue t
isValue _                 = False
