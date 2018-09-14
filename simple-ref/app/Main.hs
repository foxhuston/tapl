{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.Environment (getArgs)
import Data.Bifunctor (second)

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

    case toks of
        (Left err) -> putStrLn err
        (Right toks) -> do
            let output = parse toks
            -- print output
            -- putStrLn "---"

            case output of
                (Right (forms, PState { context, types, equations })) -> do
                    mapM_ print equations
                    putStrLn "---"
                    let equations' = map (second desugarTerm) equations
                    mapM_ print equations'
                    putStrLn "--"
                    let ctx' = generateContextFromEquations equations' types
                    putStrLn $ showContext ctx'
                    -- let (Just mainEqn) = lookup "main" equations'
                    -- putStrLn "---mainEqn---"
                    -- print mainEqn

                    mapM_ (printForm ctx' types equations') forms
                (Left error) -> putStrLn error

printForm :: Context -> TypeContext -> EqnContext -> Term -> IO ()
printForm context typeContext equations term = do
    let term' = desugarTerm term
    putStr $ showTermInContext context term'
    putStr ": "
    print $ typeOf context typeContext term'
    putStr "\n= "
    -- putStr "main = "
    putStrLn $ showTermInContext context $ eval equations term'
    putStrLn "---\n"

