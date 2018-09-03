{-# LANGUAGE NamedFieldPuns #-}

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

    -- putStrLn "---"
    -- print toks
    -- putStrLn "---"

    let output = parse toks
    print output
    putStrLn "---"

    case output of
        (Right (forms, context)) -> mapM_ (printForm context) forms
        (Left error) -> putStrLn error

printForm :: PState -> Term -> IO ()
printForm pstate term = do
    let PState { types, equations } = pstate
    let ctx' = generateContextFromEquations equations

    putStr $ showTermInContext ctx' term
    putStr ": "
    print $ typeof ctx' term
    putStr "\n= "
    putStrLn $ showTermInContext [] $ eval term
    putStrLn "---\n"
