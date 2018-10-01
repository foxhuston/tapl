{
module Parse.Tokenize (
  Lexeme (..),
  LexemeClass (..),
  tokenize
) where

}

%wrapper "monad"

$digit   = 0-9                  -- digits

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic = $printable

$idchar  = [$alpha $digit \']

$special = [\(\)\,\.\:\<\>\=\[\]\`\|\{\}\_]

@string = \" ($graphic # \") * \"

@reservedid = 
  if|then|else|true|false|succ|pred|iszero|let|in|as|of|case|type|ref|equals
  
@reservedop =
  "->" | "\" | ";" | "=>" | ":=" | "!"

@varid  = $small $idchar*
@typeid = $large $idchar*

tokens :-

<0>  $white+                          ;
<0>  "--".*                           ;
<0>  $special                         { mkL LexSpecial }
<0>  @reservedid                      { mkL LexReservedWord }
<0>  @reservedop                      { mkL LexReservedOp }
<0>  @string                          { mkL LexString }
<0>  [0-9]+                           { mkL LexNat }
<0>  @varid                           { mkL LexIdent }
<0>  @typeid                          { mkL LexTypeIdent }

{
-- Each action has type :: String -> Token

data Lexeme = L AlexPosn LexemeClass String
  deriving (Show)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p,_,_,str) len = return (L p c (take len str))

-- The token type:
data LexemeClass =
  LexSpecial
  | LexReservedWord
  | LexReservedOp
  | LexIdent
  | LexTypeIdent
  | LexNat
  | LexString
  | LEOF
  deriving (Eq,Show)

alexEOF = return (L undefined LEOF "")

scanner str = runAlex str $ do
  let loop toks = do {
    tok@(L _ cr _) <- alexMonadScan;
    case cr of
      LEOF -> return toks
      _    -> loop $! (toks ++ [tok])
  }

  loop []

tokenize = scanner
}
