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
    let toks = tokenize contents
    let forms = parse toks

    -- print toks
    putStrLn "---"
    print forms
    putStrLn "---"

    case forms of
        (Right forms) -> mapM_ printForm forms
        (Left error) -> putStrLn error

printForm :: Term -> IO ()
printForm term = do
    putStr $ showTermInContext [] term
    putStr ": "
    print $ typeof [] term
    putStr "\n= "
    putStrLn $ showTermInContext [] $ eval term
    putStrLn "---\n"
