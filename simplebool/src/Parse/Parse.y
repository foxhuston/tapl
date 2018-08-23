{
module Parse.Parse (
    parse
) where

import Parse.Tokenize
import Data.Terms

}

%name parse
%tokentype { Lexeme }

%token 
    if              { LexIf }
    then            { LexThen }
    else            { LexElse }
    true            { LexTrue }
    false           { LexFalse }
    ident           { LexIdent $$ }
    '('             { LexLParen }
    ')'             { LexRParen }
    '.'             { LexDot }
    ':'             { LexHasType }
    lam             { LexLambda }
    bool            { LexBool }
    '->'            { LexArrow }

-- %monad { Either String } { (>>=) } { return }
%error { parseError }

%%

Expr : '(' Expr ')'                 { $2 }
     | if Expr then Expr else Expr  { TermIf Blank $2 $4 $6 }
     | lam ident ':' Type '.' Expr  { TermAbs Blank $2 $4 $6 }
     | Expr Expr                    { TermApp Blank $1 $2 }
     | true                         { TermTrue Blank }
     | false                        { TermFalse Blank }
     | ident                        { TermVar Blank 0 }

Type : Type '->' Type               { TypeArrow $1 $3 }
     | bool                         { TypeBool }

{
    parseError _ = error "Parse Error"
}