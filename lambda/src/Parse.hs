module Parse (
    parseUntypedLambda
) where

import Text.Parsec
import Text.Parsec.Char (char, string, spaces, letter)
import Text.Parsec.Combinator (many1, choice, chainl1, endBy1, option)

import Data.Terms

parseUntypedLambda :: String -> Either ParseError (Context, Term)
parseUntypedLambda = runParser begin ([], []) "some-file"

-- program = sepBy term spaces

begin :: Parsec String (Context, Context) (Context, Term)
begin = do
    t <- expr
    (free, _) <- getState
    return (free, t)

expr :: Parsec String (Context, Context) Term
expr = do
    spaces
    t1 <- term
    t2 <- option t1 ((\t2' -> TermApp Blank t1 t2') <$> (spaces *> expr))
    spaces
    return t2

term :: Parsec String (Context, Context) Term
term =
    (try parens)
    <|> (try termAbs)
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

    -- Push new var name onto state
    (free, bound) <- getState
    putState $ (free, bound ++ [(varName, NameBind)])

    t1 <- expr

    (free, bound) <- getState
    putState $ (free, reverse $ tail $ reverse bound)

    return $ TermAbs Blank varName t1

termVar = do
    varName <- many1 letter

    (free, bound) <- getState
    idx <- case getIndexFromContext bound varName of
                (Just idx) -> return idx
                Nothing -> do
                    let newFree = (varName, NameBind) : free
                    putState (newFree, bound)
                    return $ (length newFree) + (length bound) - 1

    ctx <- getState

    return $ TermVar Blank (length ctx) idx
