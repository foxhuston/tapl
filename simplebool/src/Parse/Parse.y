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
    '('             { LexLParen }
    ')'             { LexRParen }
    '.'             { LexDot }
    ':'             { LexHasType }
    lam             { LexLambda }
    bool            { LexBool }
    '->'            { LexArrow }

%monad { P }
%error { parseError }

%%

Expr : '(' AppExpr ')'                         { $2 }
     | lam ident ':' Type '.' AppExpr          {% processAbs $2 $4 $6 }
     | if AppExpr then AppExpr else AppExpr    { TermIf Blank $2 $4 $6 }
     | true                                    { TermTrue Blank }
     | false                                   { TermFalse Blank }
     | ident                                   {% processVar $1 }
     -- | ident                                   { TermVar Blank 0 }

AppExpr : Expr Expr                            { TermApp Blank $1 $2 }
        | Expr                                 { $1 }

Type : Type '->' Type               { TypeArrow $1 $3 }
     | bool                         { TypeBool }

{

data PState = PState {
        context :: Context
    }
    deriving (Show)

type P a = StateT PState (Either String) a

parseError _ = lift $ Left "Parse Error"

processAbs :: String -> TermType -> Term -> P Term
processAbs ident tt subterm = do
    pstate <- get
    let ctx = context pstate
    put $ PState { context = ctx ++ [(ident, VarBind tt)] }
    return $ TermAbs Blank ident tt (TermVar Blank 0)

processVar :: String -> P Term
processVar ident = do
    pstate <- get
    let ctx = context pstate
    case getIndexFromContext ctx ident of
        Nothing -> lift $ Left ("Could not find " ++ ident ++ " in context " ++ (show ctx))
        (Just idx) -> return $ TermVar Blank idx

parse l = runStateT (expr l) (PState [])
}