module Data.Terms (
    Term(..),
    Info(..),
    isNumericValue,
    isValue
) where

data Info =
    Info {
        line:: Int,
        column:: Int
    }
    | Blank

instance Show Info where
    show (Info line column) = "{ line: " ++ (show line) ++ ", column: " ++ (show column) ++ "}"
    show Blank = ""

data Term = 
    TermTrue Info
    | TermFalse Info
    | TermIf Info Term Term Term
    | TermZero Info
    | TermSucc Info Term
    | TermPred Info Term
    | TermIsZero Info Term

genIndent :: Int -> String
genIndent n = take n $ repeat ' '

instance Show Term where
    show = showTerm 0
        where
            showTerm _ (TermTrue i) =
                "True" ++ show i
            showTerm _ (TermFalse i) =
                "False" ++ show i
            showTerm _ (TermZero i) =
                "0" ++ show i
            showTerm level (TermSucc i t) =
                "\n" ++ (genIndent level)
                     ++ "(Succ " ++ show i ++ showTerm (level + 1) t ++ ")"
            showTerm level (TermPred i t) =
                "\n" ++ (genIndent level)
                     ++ "(Pred " ++ show i ++ showTerm (level + 1) t ++ ")"
            showTerm level (TermIsZero i t) =
                "\n" ++ (genIndent level)
                     ++ "(IsZero " ++ show i ++ showTerm (level + 1) t ++ ")"
            showTerm level (TermIf i t1 t2 t3) =
                "\n" ++ (genIndent level)
                     ++ "(If " ++ show i ++ showTerm (level + 1) t1
                     ++ " then " ++ showTerm (level + 1) t2
                     ++ " else " ++ showTerm (level + 1) t3 ++ ")"

isNumericValue :: Term -> Bool
isNumericValue (TermZero _) = True
isNumericValue (TermSucc _ t) = isNumericValue t
isNumericValue (TermPred _ t) = isNumericValue t
isNumericValue _ = False

isValue :: Term -> Bool
isValue (TermFalse _) = True
isValue (TermZero _) = True
isValue t = isNumericValue t