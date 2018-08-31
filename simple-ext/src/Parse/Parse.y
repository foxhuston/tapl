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
    '->'            { LexArrow }

%monad { P }
%error { parseError }

%%

Program : AppExpr                                           { [$1] }
        | AppExpr ClearContext ';'                          { [$1] }
        | AppExpr ClearContext ';' Program                  { ($1 : $4) }

ClearContext : {- empty -}                                  {% clearContext }
PopContext : {- empty -}                                    {% popContext }

AppExpr : AppExpr Expr                                      { TermApp Blank $1 $2 }
        | Expr                                              { $1 }

Expr : '(' AppExpr ')'                                      { $2 }
     | '(' TupExpr ')'                                      { TermTup Blank $2 }
     | '{' RecordExpr '}'                                   { TermRecord Blank $2 }
     | lam TypedId '.' AppExpr PopContext                   { TermAbs Blank (fst $2) (snd $2) $4 }
     | if AppExpr then AppExpr else AppExpr                 { TermIf Blank $2 $4 $6 }
     | let MatchExpr '=' AppExpr in AppExpr                 { TermLet Blank $2 $4 $6 }
     | iszero AppExpr                                       { TermIsZero Blank $2 }
     | succ AppExpr                                         { TermSucc Blank $2 }
     | pred AppExpr                                         { TermPred Blank $2 }
     | AppExpr '.' ident                                    { TermRecordProjection Blank $1 $3 }
     | AppExpr '.' nat                                      { TermTupProjection Blank $1 $3 }
     | true                                                 { TermTrue Blank }
     | false                                                { TermFalse Blank }
     | nat                                                  { TermNat Blank $1 }
     | ident                                                {% processVar $1 }

TupExpr : {- empty -}                                       { [] }
        | AppExpr                                           { [$1] }
        | AppExpr ',' TupExpr                               { $1 : $3 }

RecordExpr : ident '=' AppExpr                              { [($1, $3)] }
           | ident '=' AppExpr ',' RecordExpr               { ($1, $3):$5 }

MatchExpr : '{' RecordPattern '}'                           { MatchRecord $2 }
          | ident                                           {% (storeAbsIdent $1 TypeUnspecified) >> return (MatchVar $1) }

RecordPattern : ident '=' MatchExpr                         { [($1, $3)] }
              | ident '=' MatchExpr ',' RecordPattern       { ($1, $3):$5 }

TypedId : ident ':' Type                                    {% storeAbsIdent $1 $3 }

Type : Type '->' Type                                       { TypeArrow $1 $3 }
     | '(' TupleType ')'                                    { TypeTuple $2 }
     | '{' RecordType '}'                                   { TypeRecord $2 }
     | boolType                                             { TypeBool }
     | natType                                              { TypeNat }

TupleType : {- empty -}                                     { [] }
          | Type                                            { [$1] }
          | Type ',' TupleType                              { $1 : $3 }

RecordType : ident ':' Type                                 { [($1, $3)] }
           | ident ':' Type ',' RecordType                  { ($1, $3):$5 }

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

popContext :: P ()
popContext = do
    pstate <- get
    let (c:cs) = context pstate
    put $ pstate { context = cs }
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