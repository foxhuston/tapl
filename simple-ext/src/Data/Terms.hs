module Data.Terms (
    TermType (..),
    Term(..),
    MatchPattern(..),
    Info(..),
    Binding(..),
    Context(..),
    TypeContext(..),
    addBinding,
    showTermInContext,
    getIndexFromContext,
    getTypeFromContext,
    getRecordType,
    isValue
) where

import Data.List (findIndex, intercalate, foldl')
import Data.Bifunctor (second)
import Data.Maybe (isJust)

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
    show (TypeTuple ts)    = "(" ++ intercalate ", " (map show ts) ++ ")"
    show (TypeRecord ts)   = showRecord ":" ts
    show (TypeArrow t1 t2) = (show t1) ++ "->" ++ (show t2)

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