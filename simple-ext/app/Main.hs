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
    -- print output
    -- putStrLn "---"

    case output of
        (Right (forms, PState { types, equations })) -> do
            let context = generateContextFromEquations equations types
            putStrLn $ showContext context
            mapM_ (printForm context types) forms
        (Left error) -> putStrLn error

printForm :: Context -> TypeContext -> Term -> IO ()
printForm context typeContext term = do
    putStr $ showTermInContext context term
    putStr ": "
    print $ desugarTypes typeContext term
    print $ typeOf context typeContext term
    putStr "\n= "
    putStrLn $ showTermInContext context $ eval term
    putStrLn "---\n"

