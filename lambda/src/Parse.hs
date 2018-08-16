module Parse (
    parseUntypedLambda
) where

import Text.Parsec
import Text.Parsec.Char (char, string, spaces, letter)
import Text.Parsec.Combinator (many1, choice, chainl1, endBy1, option)

import Data.Terms


parseUntypedLambda :: String -> Either ParseError (Context, Term)
parseUntypedLambda s = do
    parse <- runParser expr [] "some-file" s
    return $ ([], parse)

-- program = sepBy term spaces

expr :: Parsec String Context Term
expr = do
    t1 <- term
    t2 <- option (return t1) ((\t2' -> pure $ TermApp Blank t1 t2') <$> (spaces *> expr))
    return t2

term :: Parsec String Context Term
term =
    parens
    <|> termAbs
    <|> termVar

parens = do
    char '('
    spaces
    t <- expr
    spaces
    char ')'
    return t

termAbs = do
    char '\\'
    varName <- endBy1 letter (char '.')
    t1 <- expr
    return $ TermAbs Blank varName t1

termVar = do
    varName <- many1 letter
    return $ TermVar Blank 0 0
