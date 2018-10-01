{
{-# LANGUAGE NamedFieldPuns #-}

module Parse.Parse (
    parse,
    PState(..)
) where

import Data.Maybe (isJust, fromJust)
import Data.Bifunctor (first, second)

import Parse.Tokenize
import Data.Terms
import Control.Monad.State.Strict

import Debug.Trace

}

-- TODO[GH]: How do I make a two pass parser??? Do I just make two parser files?

%name parseProgram Program
%tokentype { Lexeme }

%token 
    if              { L _ LexReservedWord "if" }
    then            { L _ LexReservedWord "then" }
    else            { L _ LexReservedWord "else" }
    true            { L _ LexReservedWord "true" }
    false           { L _ LexReservedWord "false" }
    succ            { L _ LexReservedWord "succ" }
    pred            { L _ LexReservedWord "pred" }
    iszero          { L _ LexReservedWord "iszero" }
    let             { L _ LexReservedWord "let" }
    in              { L _ LexReservedWord "in" }
    type            { L _ LexReservedWord "type" }
    case            { L _ LexReservedWord "case" }
    of              { L _ LexReservedWord "of" }
    as              { L _ LexReservedWord "as" }
    ref             { L _ LexReservedWord "ref" }
    equals          { L _ LexReservedWord "equals" }
    lam             { L _ LexReservedOp "\\" }
    ':='            { L _ LexReservedOp ":=" }
    '->'            { L _ LexReservedOp "->" }
    '=>'            { L _ LexReservedOp "=>" }
    ';'             { L _ LexReservedOp ";" }
    '!'             { L _ LexReservedOp "!" }
    '('             { L _ LexSpecial "(" }
    ')'             { L _ LexSpecial ")" }
    '{'             { L _ LexSpecial "{" }
    '}'             { L _ LexSpecial "}" }
    '='             { L _ LexSpecial "=" }
    '.'             { L _ LexSpecial "." }
    ','             { L _ LexSpecial "," }
    ':'             { L _ LexSpecial ":" }
    '<'             { L _ LexSpecial "<" }
    '>'             { L _ LexSpecial ">" }
    '|'             { L _ LexSpecial "|" }
    '_'             { L _ LexSpecial "_" }
    boolType        { L _ LexTypeIdent "Bool" }
    natType         { L _ LexTypeIdent "Nat" }
    stringType      { L _ LexTypeIdent "String" }
    refType         { L _ LexTypeIdent "Ref" }
    userType        { L _ LexTypeIdent $$ }

    ident           { L _ LexIdent $$ }
    nat             { L _ LexNat $$ }
    string          { L _ LexString $$ }

%monad { P }
%error { parseError }

%%

Program : TopLevelExpression ';'                            { [$1] }
        | TopLevelExpression ';' Program                    { ($1 : $3) }


TopLevelExpression : TypeDecl                               { Nothing }
                   | Equation                               { Nothing }
                   | AppExpr                                { Just $1 }
                --    | Expr                                   { Just $1 }

TypeDecl : type userType '=' Type                           {% storeTypeContext $2 $4 }
Equation : ident '=' AppExpr                                {% storeEquation $1 $3 }


PopContext : {- empty -}                                    {% popContext }
PushMatchContext : {- empty -}                              {% pushMatchContext }
WriteMatchContext : {- empty -}                             {% writeMatchContext }

SeqExpr : SeqExpr ';' AppExpr                               { TermSequence Blank $1 $3 }
        | AppExpr                                           { $1 }

AppExpr : AppExpr Expr                                      { TermApp Blank $1 $2 }
        | Expr                                              { $1 }

Expr : '(' SeqExpr ')'                                      { $2 }
     | CaseExpr                                             { $1 }
     | LetExpr                                              { $1 }
     | RefExpr                                              { $1 }
     | NatExpr                                              { $1 }
     | TupleExpr                                            { $1 }
     | BoolExpr                                             { $1 }
     | Vals                                                 { $1 }


-- Expr : LetExpr                                             { $1 }
--      | CaseExpr                                            { $1 }
--      | RecordExpr                                          { $1 }
--      | TupleExpr                                           { $1 }
--      | BoolExpr                                            { $1 }
--      | RefExpr                                             { $1 }
--      | NatExpr                                              { $1 }
--      | Vals                                                 { $1 }

Vals : LamExpr                                              { $1 }
     | NatVal                                               { $1 }
     | RecordVal                                            { $1 }
     | TupVal                                               { $1 }
     | StringVal                                            { $1 }
     | VariantVal                                           { $1 }
     | VarExpr                                              { $1 }

-- SeqAppExpr : AppExpr ';' SeqAppExpr                         { TermSequence Blank $1 $3}
--            | AppExpr                                        { $1 }


-- Expr : SeqExpr                                              { $1 }
--      | Expr1                                                { $1 }

-- SeqExpr : '(' SeqAppExpr ')'                                { $2 }
--         | Expr2                                             { $1 }

-- Expr1 : LamExpr                                             { $1 }

-- Expr2 : RefExpr                                             { $1 }
--       | NatExpr                                             { $1 }
--       | RecordExpr                                          { $1 }
--       | TupleExpr                                           { $1 }
--       | BoolExpr                                            { $1 }
--       | StringExpr                                          { $1 }
--       | VarExpr                                             { $1 }


LamExpr : lam TypedId '.' AppExpr PopContext                { TermAbs Blank (fst $2) (snd $2) $4 }

LetExpr : let PushMatchContext MatchExpr
            '=' Expr in WriteMatchContext Expr
            PopContext                                      { TermLet Blank $3 $5 $8 }

VariantVal : '<' ident '=' AppExpr '>' as Type             { TermTag Blank $2 $4 $7 }

CaseExpr : case Expr of CaseBranches                     { TermCase Blank $2 $4 }

NatExpr : iszero Expr                                    { TermIsZero Blank $2 }
        | succ Expr                                      { TermSucc Blank $2 }
        | pred Expr                                      { TermPred Blank $2 }

RecordExpr : RecordExpr '.' ident                           { TermRecordProjection Blank $1 $3 }
           | VarExpr '.' ident                              { TermRecordProjection Blank $1 $3 }

TupleExpr : TupVal '.' nat                                  { TermTupProjection Blank $1 (read $3) }
          | VarExpr '.' nat                                 { TermTupProjection Blank $1 (read $3) }

BoolExpr : if AppExpr then AppExpr else AppExpr             { TermIf Blank $2 $4 $6 }
         | equals Expr Expr                                 { TermEquals Blank $2 $3 }
         | true                                             { TermTrue Blank }
         | false                                            { TermFalse Blank }

StringVal : string                                         { TermString Blank (trimLexString $1) }


RefExpr : ref Expr                                       { TermRef Blank $2 }
        | VarExpr ':=' Expr                              { TermBecomes Blank $1 $3 }
        | '!' Expr                                       { TermDeref Blank $2 }

VarExpr : ident                                             {% processVar $1 }

NatVal : nat                                               { TermNat Blank (read $1) }

TupVal : '(' ')'                                           { TermTup Blank [] }
       | '(' TupInner ')'                                  { TermTup Blank $2 }

TupInner : SeqExpr ',' TupInner                             { $1 : $3 }
         | SeqExpr ',' SeqExpr                              { [$1, $3] }

RecordVal : '{' RecordInner '}'                            { TermRecord Blank $2 }

RecordInner : ident '=' AppExpr                             { [($1, $3)] }
            | ident '=' AppExpr ',' RecordInner             { ($1, $3):$5 }

MatchExpr : '{' RecordPattern '}'                           { MatchRecord $2 }
          | ident                                           {% (storeMatchIdent (VarName $1)) >> return (MatchVar $ VarName $1) }

RecordPattern : ident '=' MatchExpr                         { [($1, $3)] }
              | ident '=' MatchExpr ',' RecordPattern       { ($1, $3):$5 }


TypedId : ident ':' Type                                    {% storeAbsIdent (VarName $1) $3 }
        | '_' ':' Type                                      {% storeAbsIdent WildCard $3 }

CaseBranches : CaseBranch                                   { [$1] }
             | CaseBranches '|' CaseBranch                  { ($3 : $1) }

CaseBranch : CaseBranchTag '=>'
             WriteMatchContext
             Expr
             PopContext                                     { (CaseTag (fst $1) (snd $1), $4) }

CaseBranchTag : '<' ident PushMatchContext '=' ident '>'    {% (storeMatchIdent (VarName $5)) >> return ($2, (VarName $5)) }
              | '<' ident PushMatchContext '=' '_' '>'      {% (storeMatchIdent WildCard) >> return ($2, WildCard) }

-- Types

Type : '(' Type ')'                                         { $2 }
     | Type '->' Type                                       { TypeArrow $1 $3 }
     | '(' TupleType ')'                                    { TypeTuple $2 }
     | '{' RecordType '}'                                   { TypeRecord $2 }
     | '<' RecordType '>'                                   { TypeVariant $2 }
     | refType Type                                         { TypeRef $2 }
     | boolType                                             { TypeBool }
     | natType                                              { TypeNat }
     | stringType                                           { TypeString }
     | userType                                             { TypeUser $1 }

TupleType : {- empty -}                                     { [] }
          | Type ',' TupleType                              { $1 : $3 }
          | Type ',' Type                                   { [$1, $3] }

RecordType : ident ':' Type                                 { [($1, $3)] }
           | ident ':' Type ',' RecordType                  { ($1, $3):$5 }

{

trimLexString :: String -> String
trimLexString = tail . init

traceShowMsg :: Show a => String -> a -> a
traceShowMsg msg x = let
    s = msg ++ ": " ++ show x ++ "\n"
    in trace s x
-- traceShowMsg _ x = x

data PState = PState {
        context :: [Context],
        matchStack :: [Context],
        types :: TypeContext,
        equations :: EqnContext
    }
    deriving (Show)

type P a = StateT PState (Either String) a

parseError [] = lift $ Left ("Parse Error: No ; at EOF")
parseError (tok:toks) = lift $
    Left ("Parse Error at: " ++ show tok ++ "\n\n" ++ show toks)

storeTypeContext :: String -> TermType -> P ()
storeTypeContext name ty = do
    pstate <- get
    let tctx = types pstate
    put $ pstate { types = tctx ++ [(name, ty)] }
    return ()

storeEquation :: String -> Term -> P ()
storeEquation name term = do
    pstate <- get
    let ctx = context pstate
    let eqns = equations pstate
    let ps' = pstate {
        equations = (name,term) : eqns,
        context = [(VarName name, NameBind)] : ctx
    }
    put $ traceShowMsg "storeEquation" ps'
    return ()

popContext :: P ()
popContext = do
    pstate <- get
    let cs = init $ context pstate
    put $ traceShowMsg "popContext" $ pstate { context = cs }
    return ()

storeAbsIdent :: VarName -> TermType -> P (VarName, TermType)
storeAbsIdent (WildCard) tt = do
    pstate <- get
    let ctx = context pstate
    put $ traceShowMsg "storeAbsIdent wc" $ pstate { context = ctx ++ [[(WildCard, NameBind)]] }
    return (WildCard, tt)

storeAbsIdent (VarName ident) tt = do
    pstate <- get
    let ctx = context pstate
    put $ traceShowMsg ("storeAbsIdent " ++ ident) $ pstate { context = ctx ++ [[(VarName ident, NameBind)]] }
    return (VarName ident, tt)

writeMatchContext :: P ()
writeMatchContext = do
    pstate <- get
    let PState { context, matchStack } = pstate
    let (c:cs) = matchStack
    put $ traceShowMsg "writeMatchContext" $ pstate { context = context ++ [c], matchStack = cs }

pushMatchContext :: P ()
pushMatchContext = do
    pstate <- get
    let cms = matchStack pstate
    put $ traceShowMsg "pushMatchContext" $ pstate { matchStack = []:cms }

storeMatchIdent :: VarName -> P ()
storeMatchIdent (WildCard) = do
    return ()

storeMatchIdent (VarName ident) = do
    pstate <- get
    let (cms:cs) = matchStack pstate
    let cms' = cms ++ [(VarName ident, NameBind)]

    put $ traceShowMsg "storeMatchIdent" $ pstate { matchStack = cms':cs }

processVar :: String -> P Term
processVar ident = do
    pstate <- get
    let ctx = traceShowMsg ("processVar " ++ ident) $ concat $ context pstate
    case getIndexFromContext ctx ident of
        Nothing -> lift $ Left ("Could not find " ++ ident ++ " in context " ++ (show ctx))
        (Just idx) -> return $ traceShowMsg "pv Found" $ TermVar Blank idx

unMaybeFirst :: ([Maybe a], b) -> ([a], b)
unMaybeFirst = first (map fromJust . filter isJust)

revEqns :: PState -> PState
revEqns p@(PState { equations }) = p { equations = reverse equations }

parse' :: [Lexeme] -> Either String ([Maybe Term], PState)
parse' l = runStateT (parseProgram l) (PState [] [] [] [])

parse l = unMaybeFirst <$> parse' l
}
