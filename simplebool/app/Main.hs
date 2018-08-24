module Main where

import System.Environment (getArgs)

import Lib


-- str = "(\\x:Bool->Bool. if x false then true else false)\n(\\x:Bool. if x then false else true)"

-- main :: IO ()
-- main = do
--     putStrLn $ str ++ " -> "
--     let toks = tokenize str
--     print toks
--     print . parse $ toks

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> runFile fileName
        _ -> putStrLn "Usage: untyped-arith-exe <file-name>"

-- runFile :: String -> IO ()
-- runFile fileName = do
--     contents <- readFile fileName
--     let forms = parseSimpleBool contents
--     print forms

runFile :: String -> IO ()
runFile fileName = do
    contents <- readFile fileName
    let forms = parse . tokenize $ contents
    case forms of
        (Right forms) -> mapM_ printForm forms
        (Left error) -> putStrLn error

printForm :: Term -> IO ()
printForm term = do
    print term
    putStrLn "---"
    putStr $ showTermInContext [] term
    putStr " -> "
    putStrLn $ showTermInContext [] $ eval term
    putStrLn ""
