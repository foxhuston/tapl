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
  "->"                             { \s -> LexArrow }
  "\"                              { \s -> LexLambda }
  "."                              { \s -> LexDot }
  ":"                              { \s -> LexHasType }
  ";"                              { \s -> LexSep }
  "("                              { \s -> LexLParen }
  ")"                              { \s -> LexRParen }
  [Bb]"ool"                        { \s -> LexBool }
  $alpha [$alpha $digit \_ \']*    { \s -> LexIdent s }

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
  | LexBool
  | LexIdent String
  deriving (Eq,Show)

tokenize = alexScanTokens
}
