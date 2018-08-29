module Data.Terms (
    TermType (..),
    Term(..),
    Info(..),
    Binding(..),
    Context(..),
    addBinding,
    showTermInContext,
    getIndexFromContext,
    getTypeFromContext,
    isValue
) where

import Data.List (findIndex, intercalate)
import Data.Maybe (isJust)

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
      TypeBool
    | TypeNat
    | TypeTuple [TermType]
    | TypeArrow TermType TermType
    deriving (Eq)

instance Show TermType where
    show (TypeNat)         = "Nat"
    show (TypeBool)        = "Bool"
    show (TypeTuple ts)    = "(" ++ intercalate ", " (map show ts) ++ ")"
    show (TypeArrow t1 t2) = (show t1) ++ "->" ++ (show t2)

data Binding =
      NameBind
    | VarBind TermType
    deriving (Show)

data Term = 
      TermTrue Info
    | TermFalse Info
    | TermIf Info Term Term Term
    | TermVar Info Int
    | TermAbs Info String TermType Term
    | TermApp Info Term Term
    | TermSucc Info Term
    | TermPred Info Term
    | TermIsZero Info Term
    | TermNat Info Integer
    | TermTup Info [Term]
    | TermTupProjection Info Term Integer
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
showTermInContext _ (TermNat _ n) = show n
showTermInContext _ (TermTrue _)  = "true"
showTermInContext _ (TermFalse _) = "false"

isValue :: Term -> Bool
isValue (TermAbs _ _ _ _) = True
isValue (TermTrue _)      = True
isValue (TermFalse _)     = True
isValue (TermNat _ _)     = True
isValue (TermTup _ [])    = True
isValue (TermTup _ ts)    = all isValue ts
isValue _                 = False