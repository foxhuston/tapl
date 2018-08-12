module Parse (
    parseUntypedArith
) where

import Text.Parsec
import Text.Parsec.Char (char, string, spaces)
import Text.Parsec.Combinator (many1, choice, chainl1)

import Data.Terms

parseUntypedArith :: String -> Either ParseError [Term]
parseUntypedArith = parse program "some-file"

program = sepBy term spaces

term =
    parens
    <|> (try termZero)
    <|> (try termIf)
    <|> termTrue
    <|> termFalse
    <|> termIsZero
    <|> termSucc
    <|> termPred

parens = do
    char '('
    spaces
    t <- term
    spaces
    char ')'
    return t

termZero = char '0' >> (return $ TermZero Blank)
termTrue = string "true" >> (return $ TermTrue Blank)
termFalse = string "false" >> (return $ TermFalse Blank)

termIsZero = string "iszero" >> spaces >> term >>= (\t -> return $ TermIsZero Blank t)
termSucc = string "succ" >> spaces >> term >>= (\t -> return $ TermSucc Blank t)
termPred = string "pred" >> spaces >> term >>= (\t -> return $ TermPred Blank t)

termIf = do
    string "if"
    spaces
    t1 <- term
    spaces
    string "then"
    spaces
    t2 <- term
    spaces
    string "else"
    spaces
    t3 <- term
    return $ TermIf Blank t1 t2 t3