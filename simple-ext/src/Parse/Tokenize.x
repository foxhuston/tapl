{
module Parse.Tokenize (
  Lexeme (..),
  tokenize
) where

}

%wrapper "basic"

$digit = 0-9                  -- digits
$alpha = [a-zA-Z]             -- alphabetic characters

tokens :-
  $white+                          ;
  "--".*                           ;
  if                               { \s -> LexIf }
  then                             { \s -> LexThen }
  else                             { \s -> LexElse }
  true                             { \s -> LexTrue }
  false                            { \s -> LexFalse }
  succ                             { \s -> LexSucc }
  pred                             { \s -> LexPred }
  "->"                             { \s -> LexArrow }
  "\"                              { \s -> LexLambda }
  "."                              { \s -> LexDot }
  ":"                              { \s -> LexHasType }
  ";"                              { \s -> LexSep }
  "("                              { \s -> LexLParen }
  ")"                              { \s -> LexRParen }
  [0-9]+                           { \s -> LexNat $ read s }
  [a-z][A-Za-z0-9_']*              { \s -> LexIdent s }
  [A-Z][A-Za-z0-9_']*              { \s -> LexTypeIdent s }

{
-- Each action has type :: String -> Token

-- The token type:
data Lexeme =
    LexIf
  | LexThen
  | LexElse
  | LexTrue
  | LexFalse
  | LexLambda
  | LexSep
  | LexArrow
  | LexDot
  | LexLParen
  | LexRParen
  | LexHasType
  | LexNat Integer
  | LexSucc
  | LexPred
  | LexIdent String
  | LexTypeIdent String
  deriving (Eq,Show)

tokenize = alexScanTokens
}
