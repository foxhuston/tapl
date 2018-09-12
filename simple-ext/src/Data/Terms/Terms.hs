module Data.Terms.Terms (
    Binding(..),
    CaseTag(..),
    Context(..),
    EqnContext(..),
    Info(..),
    MatchPattern(..),
    Term(..),
    TermType (..),
    TypeContext(..),
    VarName(..)
)
where

import Data.List (intercalate)

-- data Term a = ... | TermVar Info a | ...
data Term = 
      TermTrue Info
    | TermFalse Info
    | TermIf Info Term Term Term
    | TermLet Info MatchPattern Term Term
    | TermVar Info Int
    | TermAbs Info VarName TermType Term
    | TermApp Info Term Term
    | TermSucc Info Term
    | TermPred Info Term
    | TermIsZero Info Term
    | TermNat Info Integer
    | TermString Info String
    | TermTag Info String Term TermType
    | TermCase Info Term [(CaseTag, Term)]
    | TermTup Info [Term]
    | TermTupProjection Info Term Integer
    | TermRecord Info [(String, Term)]
    | TermRecordProjection Info Term String
    | TermSequence Info Term Term
    | TermRef Info Term
    | TermBecomes Info Term Term
    | TermDeref Info Term
    deriving (Show, Eq)
    
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
    | TypeVariant [(String, TermType)]
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
    show (TypeVariant ts)  = "<" ++ (showRecord ":" ts) ++ ">"
    show (TypeRecord ts)   = "{" ++ (showRecord ":" ts) ++ "}"
    show (TypeArrow t1 t2) = (show t1) ++ "->" ++ (show t2) 
data Binding =
      NameBind
    | VarBind TermType
    deriving (Show)

data VarName =
      VarName String
    | WildCard
    deriving (Eq)

instance Show VarName where 
    show (VarName n) = n
    show (WildCard)  = "_"

data MatchPattern =
      MatchVar VarName
    | MatchRecord [(String, MatchPattern)]
    deriving (Eq)

instance Show MatchPattern where
    show (MatchVar s) = show s
    show (MatchRecord rs) = "{" ++ (showRecord "=" rs) ++ "}"

data CaseTag = CaseTag { caseLabel :: String, caseVar :: VarName }
    deriving (Show, Eq)


type Context = [(VarName, Binding)]
type EqnContext = [(String, Term)]

showRecord :: Show s => String -> [(String, s)] -> String
showRecord sep ts = intercalate ", " (map (\(l, s) -> l ++ sep ++ (show s)) ts)
