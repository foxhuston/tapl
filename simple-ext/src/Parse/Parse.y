{
module Parse.Parse (
    parse
) where

import Parse.Tokenize
import Data.Terms
import Control.Monad.State.Strict

}

%name expr
%tokentype { Lexeme }

%token 
    if              { LexIf }
    then            { LexThen }
    else            { LexElse }
    true            { LexTrue }
    false           { LexFalse }
    ident           { LexIdent $$ }
    nat             { LexNat $$ }
    succ            { LexSucc }
    pred            { LexPred }
    '('             { LexLParen }
    ')'             { LexRParen }
    '.'             { LexDot }
    ':'             { LexHasType }
    ';'             { LexSep }
    lam             { LexLambda }
    boolType        { LexTypeIdent "Bool" }
    natType         { LexTypeIdent "Nat" }
    '->'            { LexArrow }

%monad { P }
%error { parseError }

%%

Program : AppExpr                              { [$1] }
        | AppExpr ClearContext ';'             { [$1] }
        | AppExpr ClearContext ';' Program     { ($1 : $4) }

ClearContext : {- empty -}                     {% clearContext }

Expr : '(' AppExpr ')'                         { $2 }
     | lam TypedId '.' AppExpr ClearContext    { TermAbs Blank (fst $2) (snd $2) $4 }
     | if AppExpr then AppExpr else AppExpr    { TermIf Blank $2 $4 $6 }
     | succ AppExpr                            { TermSucc Blank $2 }
     | pred AppExpr                            { TermPred Blank $2 }
     | true                                    { TermTrue Blank }
     | false                                   { TermFalse Blank }
     | nat                                     { TermNat Blank $1 }
     | ident                                   {% processVar $1 }

AppExpr : AppExpr Expr                         { TermApp Blank $1 $2 }
        | Expr                                 { $1 }

TypedId : ident ':' Type                       {% storeAbsIdent $1 $3 }

Type : Type '->' Type                          { TypeArrow $1 $3 }
     | boolType                                { TypeBool }
     | natType                                 { TypeNat }

{

data PState = PState {
        context :: Context
    }
    deriving (Show)

type P a = StateT PState (Either String) a

parseError tok = lift $ Left ("Parse Error: " ++ show tok)

clearContext :: P ()
clearContext = do
    pstate <- get
    put $ pstate { context = [] }
    return ()

storeAbsIdent :: String -> TermType -> P (String, TermType)
storeAbsIdent ident tt = do
    pstate <- get
    let ctx = context pstate
    put $ pstate { context = ctx ++ [(ident, NameBind)] }
    return $ (ident, tt)

processVar :: String -> P Term
processVar ident = do
    pstate <- get
    let ctx = context pstate
    case getIndexFromContext ctx ident of
        Nothing -> lift $ Left ("Could not find " ++ ident ++ " in context " ++ (show ctx))
        (Just idx) -> return $ TermVar Blank idx

parse l = evalStateT (expr l) (PState [])
}