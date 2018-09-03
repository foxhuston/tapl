{
{-# LANGUAGE NamedFieldPuns #-}

module Parse.Parse (
    parse
) where

import Parse.Tokenize
import Data.Terms
import Control.Monad.State.Strict

import Debug.Trace

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
    string          { LexString $$ }
    succ            { LexSucc }
    pred            { LexPred }
    iszero          { LexIsZero }
    let             { LexLet }
    in              { LexIn }
    '('             { LexLParen }
    ')'             { LexRParen }
    '{'             { LexLBrace }
    '}'             { LexRBrace }
    '='             { LexEquals }
    '.'             { LexDot }
    ','             { LexComma }
    ':'             { LexHasType }
    ';'             { LexSep }
    lam             { LexLambda }
    boolType        { LexTypeIdent "Bool" }
    natType         { LexTypeIdent "Nat" }
    stringType      { LexTypeIdent "String" }
    userType        { LexTypeIdent $$ }
    '->'            { LexArrow }

%monad { P }
%error { parseError }

%%

Program : AppExpr                                           { [$1] }
        | AppExpr ClearContext ';'                          { [$1] }
        | AppExpr ClearContext ';' Program                  { ($1 : $4) }

ClearContext : {- empty -}                                  {% clearContext }
PopContext : {- empty -}                                    {% popContext }
WriteMatchContext : {- empty -}                             {% writeMatchContext }

AppExpr : AppExpr Expr                                      { TermApp Blank $1 $2 }
        | Expr                                              { $1 }

Expr : '(' AppExpr ')'                                      { $2 }
     | lam TypedId '.' AppExpr PopContext                   { TermAbs Blank (fst $2) (snd $2) $4 }
     | if AppExpr then AppExpr else AppExpr                 { TermIf Blank $2 $4 $6 }
     | let MatchExpr
        '=' AppExpr in WriteMatchContext AppExpr
        PopContext                                          { TermLet Blank $2 $4 $7 }
     | iszero AppExpr                                       { TermIsZero Blank $2 }
     | succ AppExpr                                         { TermSucc Blank $2 }
     | pred AppExpr                                         { TermPred Blank $2 }
     | RecordExpr '.' ident                                 { TermRecordProjection Blank $1 $3 }
     | TupExpr '.' nat                                      { TermTupProjection Blank $1 $3 }
     | VarExpr '.' ident                                    { TermRecordProjection Blank $1 $3 }
     | VarExpr '.' nat                                      { TermTupProjection Blank $1 $3 }
     -- Constants
     | TupExpr                                              { $1 }
     | RecordExpr                                           { $1 }
     | true                                                 { TermTrue Blank }
     | false                                                { TermFalse Blank }
     | nat                                                  { TermNat Blank $1 }
     | string                                               { TermString Blank $1 }
     | VarExpr                                              { $1 }


VarExpr : ident                                             {% processVar $1 }

TupExpr : '(' TupInner ')'                                  { TermTup Blank $2 }

TupInner : {- empty -}                                      { [] }
         | AppExpr                                          { [$1] }
         | AppExpr ',' TupInner                             { $1 : $3 }

RecordExpr : '{' RecordInner '}'                            { TermRecord Blank $2 }

RecordInner : ident '=' AppExpr                             { [($1, $3)] }
            | ident '=' AppExpr ',' RecordInner             { ($1, $3):$5 }

MatchExpr : '{' RecordPattern '}'                           { MatchRecord $2 }
          | ident                                           {% (storeMatchIdent $1) >> return (MatchVar $1) }

RecordPattern : ident '=' MatchExpr                         { [($1, $3)] }
              | ident '=' MatchExpr ',' RecordPattern       { ($1, $3):$5 }

TypedId : ident ':' Type                                    {% storeAbsIdent $1 $3 }

Type : Type '->' Type                                       { TypeArrow $1 $3 }
     | '(' TupleType ')'                                    { TypeTuple $2 }
     | '{' RecordType '}'                                   { TypeRecord $2 }
     | boolType                                             { TypeBool }
     | natType                                              { TypeNat }
     | stringType                                           { TypeString }

TupleType : {- empty -}                                     { [] }
          | Type                                            { [$1] }
          | Type ',' TupleType                              { $1 : $3 }

RecordType : ident ':' Type                                 { [($1, $3)] }
           | ident ':' Type ',' RecordType                  { ($1, $3):$5 }

{


-- traceShowMsg :: Show a => String -> a -> a
-- traceShowMsg msg x = let
--     s = msg ++ ": " ++ show x ++ "\n"
--     in trace s x

data PState = PState {
        context :: [Context],
        currentMatchContext :: Context
    }
    deriving (Show)

type P a = StateT PState (Either String) a

parseError tok = lift $ Left ("Parse Error: " ++ show tok)

clearContext :: P ()
clearContext = do
    pstate <- get
    put $ pstate { context = [] }
    return ()

popContext :: P ()
popContext = do
    pstate <- get
    let cs = init $ context pstate
    put $ pstate { context = cs }
    return ()

storeAbsIdent :: String -> TermType -> P (String, TermType)
storeAbsIdent ident tt = do
    pstate <- get
    let ctx = context pstate
    put $ pstate { context = ctx ++ [[(ident, NameBind)]] }
    return $ (ident, tt)

writeMatchContext :: P ()
writeMatchContext = do
    pstate <- get
    let PState { context, currentMatchContext } = pstate
    put $ pstate { context = context ++ [currentMatchContext], currentMatchContext = [] }

storeMatchIdent :: String -> P ()
storeMatchIdent ident = do
    pstate <- get
    let ctx = currentMatchContext pstate

    put $ pstate { currentMatchContext = ctx ++ [(ident, NameBind)] }

processVar :: String -> P Term
processVar ident = do
    pstate <- get
    let ctx = concat $ context pstate
    case getIndexFromContext ctx ident of
        Nothing -> lift $ Left ("Could not find " ++ ident ++ " in context " ++ (show ctx))
        (Just idx) -> return $ TermVar Blank idx

parse l = evalStateT (expr l) (PState [] [])
}