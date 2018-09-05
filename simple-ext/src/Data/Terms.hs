module Data.Terms (
    TermType (..),
    Term(..),
    MatchPattern(..),
    Info(..),
    Binding(..),
    Context(..),
    TypeContext(..),
    EqnContext(..),
    mapTerm,
    addBinding,
    showContext,
    showTermInContext,
    getIndexFromContext,
    indexToEquation,
    getTypeFromContext,
    getTypeForName,
    getNameForType,
    getRecordType,
    isValue
) where

import Data.List (findIndex, intercalate, foldl', find)
import Data.Bifunctor (first, second)
import Data.Maybe (isJust, fromJust, fromMaybe)

import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (try)

import Debug.Trace

-- (!?) :: [a] -> Int -> Maybe a
-- (!?) l n = case unsafePerformIO $ try (return $ l !! n) of
--     Right a -> Just a
--     Left _ -> Nothing

showRecord :: Show s => String -> [(String, s)] -> String
showRecord sep ts = "{" ++ intercalate ", " (map (\(l, s) -> l ++ sep ++ (show s)) ts) ++ "}"

data Info =
    Info {
        line:: Int,
        column:: Int
    }
    | Blank
    deriving (Eq)

instance Show Info where
    show (Info line column) = "{ line: " ++ (show line) ++ ", column: " ++ (show column) ++ "}"
    show Blank = ""

data TermType =
      TypeUnspecified
    | TypeBool
    | TypeNat
    | TypeString
    | TypeTuple [TermType]
    | TypeRecord [(String, TermType)]
    -- | TypeVariant [(String, TermType)]
    | TypeUser String
    | TypeArrow TermType TermType
    deriving (Eq)

type TypeContext = [(String, TermType)]

instance Show TermType where
    show (TypeNat)         = "Nat"
    show (TypeBool)        = "Bool"
    show (TypeString)      = "String"
    show (TypeUser n)      = n
    show (TypeTuple ts)    = "(" ++ intercalate ", " (map show ts) ++ ")"
    show (TypeRecord ts)   = showRecord ":" ts
    show (TypeArrow t1 t2) = (show t1) ++ "->" ++ (show t2)

getTypeForName :: TypeContext -> TermType -> TermType
getTypeForName tc t
    | (TypeUser name) <- t
    , (Just b) <- lookup name tc
    = b

    | (TypeTuple ts) <- t
    = TypeTuple $ map (getTypeForName tc) ts

    | (TypeRecord ts) <- t
    = TypeRecord $ map (second (getTypeForName tc)) ts

    | (TypeArrow t1 t2) <- t
    = TypeArrow (getTypeForName tc t1) (getTypeForName tc t2)

    | otherwise = t

getNameForType :: TypeContext -> TermType -> TermType
getNameForType tc t
    | (Just (a, _)) <- find (\(_, b) -> b == t) $ map (first TypeUser) tc
    = a

    | (TypeTuple ts) <- t
    = TypeTuple $ map (getNameForType tc) ts

    | (TypeRecord ts) <- t
    = TypeRecord $ map (second (getNameForType tc)) ts

    | (TypeArrow t1 t2) <- t
    = TypeArrow (getNameForType tc t1) (getNameForType tc t2)

    | otherwise = t

data Binding =
      NameBind
    | VarBind TermType
    deriving (Show)

data MatchPattern =
      MatchVar String
    | MatchRecord [(String, MatchPattern)]
    deriving (Eq)

instance Show MatchPattern where
    show (MatchVar s) = s
    show (MatchRecord rs) = showRecord "=" rs

data Term = 
      TermTrue Info
    | TermFalse Info
    | TermIf Info Term Term Term
    | TermLet Info MatchPattern Term Term
    | TermVar Info Int
    | TermAbs Info String TermType Term
    | TermApp Info Term Term
    | TermSucc Info Term
    | TermPred Info Term
    | TermIsZero Info Term
    | TermNat Info Integer
    | TermString Info String
    -- | TermTag Info String Term
    -- | TermCase Info Term [(Term, Term)]
    | TermTup Info [Term]
    | TermTupProjection Info Term Integer
    | TermRecord Info [(String, Term)]
    | TermRecordProjection Info Term String
    deriving (Show, Eq)

type Context = [(String, Binding)]
type EqnContext = [(String, Term)]

indexToEquation :: EqnContext -> Int -> Term
indexToEquation ctx n = snd $ ctx !! ((length ctx) - 1 - n)

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

indexToName :: Context -> Int -> String
indexToName ctx n = fst $ ctx !! ((length ctx) - 1 - n)

getIndexFromContext :: Context -> String -> Maybe Int
getIndexFromContext ctx name =
    (\idx -> (length ctx) - 1 - idx) <$>
    findIndex (\(s, _) -> name == s) ctx

getTypeFromContext :: Context -> Int -> Maybe TermType
getTypeFromContext ctx x =
    (\(VarBind tt) -> tt) <$> lookup (indexToName ctx x) ctx

addBinding :: Context -> String -> Binding -> Context
addBinding ctx s b = ctx ++ [(s, b)]

hasVar :: Context -> String -> Bool
hasVar ctx name = isJust $ getIndexFromContext ctx name

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x
    | ctx `hasVar` x
    = pickFreshName ctx (x ++ "'")

    | otherwise = (ctx ++ [(x, NameBind)], x)

pickFreshNames :: Context -> [String] -> (Context, [String])
pickFreshNames ctx xs = foldl' (\(ctx, names) name ->
    let (ctx', name') = pickFreshName ctx name
    in (ctx', name':names)) (ctx, []) xs

getMatchNames :: MatchPattern -> [String]
getMatchNames (MatchVar x) = [x]
getMatchNames (MatchRecord (r:rs)) = getMatchNames (snd r) ++ (concat $ map (getMatchNames . snd) rs)

getRecordType :: [(String, MatchPattern)] -> [(String, a)] -> [Maybe (MatchPattern, a)]
getRecordType [] _ = []
getRecordType ((label, pattern):ms) ts =
    (do
        ty <- (lookup label ts)
        return (pattern, ty)
    ):getRecordType ms ts

showContext :: Context -> String
showContext = concat . map printEntry
    where printEntry :: (String, Binding) -> String
          printEntry (name, VarBind tt) = name ++ ": " ++ show tt ++ "\n\n"
          printEntry (name, _) = error "Term " ++ name ++ " has no type in context!\n"

showTermInContext :: Context -> Term -> String
showTermInContext ctx (TermAbs _ x ty t1) =
    let (ctx', x') = pickFreshName ctx x in
        "(λ" ++ x' ++ ": " ++ (show ty) ++ ". " ++ showTermInContext ctx' t1 ++ ")"
showTermInContext ctx (TermApp _ t1 t2) =
    "(" ++ showTermInContext ctx t1 ++ " " ++ showTermInContext ctx t2 ++ ")"
showTermInContext ctx (TermVar _ n) = indexToName ctx n
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
showTermInContext ctx (TermRecord _ ts) = showRecord "=" $ map (second (showTermInContext ctx)) ts
showTermInContext ctx (TermRecordProjection _ t l) = showTermInContext ctx t ++ "." ++ l
showTermInContext ctx (TermLet _ m t1 t2) =
    let (ctx', xs') = pickFreshNames ctx $ getMatchNames m
    in "(let " ++ show m ++ " = " ++ showTermInContext ctx t1
       ++ "\nin " ++ showTermInContext ctx' t2 ++ ")"
showTermInContext _ (TermNat _ n) = show n
showTermInContext _ (TermString _ s) = show s
showTermInContext _ (TermTrue _)  = "true"
showTermInContext _ (TermFalse _) = "false"

isValue :: Term -> Bool
isValue (TermAbs _ _ _ _) = True
isValue (TermTrue _)      = True
isValue (TermFalse _)     = True
isValue (TermNat _ _)     = True
isValue (TermTup _ [])    = True
isValue (TermString _ _)  = True
isValue (TermTup _ ts)    = all isValue ts
isValue (TermRecord _ ts) = all (isValue.snd) ts
isValue _                 = False