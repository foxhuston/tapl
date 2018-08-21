module Parse (
    parseUntypedLambda
) where

import Text.Parsec
import Text.Parsec.Char (char, string, spaces, letter)
import Text.Parsec.Combinator (many1, choice, chainl1, endBy, option)

import Data.Terms

parseUntypedLambda :: String -> Either ParseError [(Context, Term)]
parseUntypedLambda = runParser program ([], []) "some-file"

program :: Parsec String (Context, Context) [(Context, Term)]
program = endBy begin (char ';' *> spaces)

begin :: Parsec String (Context, Context) (Context, Term)
begin = do
    t <- expr
    (free, _) <- getState
    putState ([], [])
    return (free, t)

expr :: Parsec String (Context, Context) Term
expr = try (term `chainl1` parseApp) <|> term

term :: Parsec String (Context, Context) Term
term =
    (try parens)
    <|> (try termAbs)
    <|> termVar

parseApp :: Parsec String (Context, Context) (Term -> Term -> Term)
parseApp = do
    spaces
    return $ TermApp Blank

parens = do
    char '('
    spaces
    t <- expr
    spaces
    char ')'
    return t

termAbs = do
    char '\\'
    varName <- endBy1 letter (spaces *> char '.')
    spaces

    -- Push new var name onto bound state stack
    (free, bound) <- getState
    putState $ (free, bound ++ [(varName, NameBind)])

    t1 <- expr

    -- Pop it off of the bound state stack
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

    return $ TermVar Blank idx
