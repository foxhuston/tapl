module Main where

import System.Environment (getArgs)

import Lib


str = "(\\x:Bool->Bool. (\\y:Bool. if x false then y else false))"

main :: IO ()
main = do
    putStrLn $ str ++ " -> "
    print . parse $ tokenize str

-- main :: IO ()
-- main = do
--     args <- getArgs
--     case args of
--         [fileName] -> runFile fileName
--         _ -> putStrLn "Usage: untyped-arith-exe <file-name>"

-- runFile :: String -> IO ()
-- runFile fileName = do
--     contents <- readFile fileName
--     let forms = parseSimpleBool contents
--     print forms

-- runFile :: String -> IO ()
-- runFile fileName = do
--     contents <- readFile fileName
--     let forms = parseUntypedLambda contents
--     case forms of
--         (Right forms) -> mapM_ printForm forms
--         (Left error) -> print error

-- printForm :: (Context, Term) -> IO ()
-- printForm (ctx, term) = do
--     putStr $ showTermInContext ctx term
--     putStr " -> "
--     putStrLn $ showTermInContext ctx $ eval term
--     putStrLn ""
