{
module Parse.Tokenize (
  Lexeme (..),
  tokenize
) where

}

%wrapper "basic"

$digit = 0-9                  -- digits
$alpha = [a-zA-Z]             -- alphabetic characters
$graphic = $printable

@string = \" ($graphic # \") * \"


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
  iszero                           { \s -> LexIsZero }
  let                              { \s -> LexLet }
  in                               { \s -> LexIn }
  as                               { \s -> LexAs }
  of                               { \s -> LexOf }
  case                             { \s -> LexCase }
  type                             { \s -> LexType }
  "->"                             { \s -> LexArrow }
  "\"                              { \s -> LexLambda }
  "."                              { \s -> LexDot }
  ","                              { \s -> LexComma }
  ":"                              { \s -> LexHasType }
  ";"                              { \s -> LexSep }
  "("                              { \s -> LexLParen }
  ")"                              { \s -> LexRParen }
  "{"                              { \s -> LexLBrace }
  "}"                              { \s -> LexRBrace }
  "<"                              { \s -> LexLBracket }
  ">"                              { \s -> LexRBracket }
  "="                              { \s -> LexEquals }
  @string                          { LexString . read }
  [0-9]+                           { LexNat . read }
  [a-z][A-Za-z0-9_']*              { LexIdent }
  [A-Z][A-Za-z0-9_']*              { LexTypeIdent }

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
  | LexComma
  | LexArrow
  | LexDot
  | LexLet
  | LexIn
  | LexCase
  | LexOf
  | LexType
  | LexAs
  | LexLBracket
  | LexRBracket
  | LexLParen
  | LexRParen
  | LexLBrace
  | LexRBrace
  | LexEquals
  | LexHasType
  | LexNat Integer
  | LexSucc
  | LexPred
  | LexIsZero
  | LexString String
  | LexIdent String
  | LexTypeIdent String
  deriving (Eq,Show)

tokenize = alexScanTokens
}
