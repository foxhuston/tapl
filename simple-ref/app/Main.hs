{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.Environment (getArgs)
import Data.Bifunctor (second)

import Lib

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
            putStrLn "-- PARSE --"
            print output
            putStrLn "---"

            case output of
                (Right (forms, PState { context, types, equations })) -> do
                    putStrLn "\n-- Equations --"
                    mapM_ print equations

                    -- putStrLn "-- Context --"
                    -- print context

                    let equations' = map (second desugarTerm) equations
                    putStrLn "\n-- Desugared --"
                    mapM_ print equations'

                    let ctx' = generateContextFromEquations equations' types
                    let (heap, evalEqn) = generateHeapFromContext equations'

                    putStrLn "\n-- Eqn Context --"
                    putStrLn $ showContext ctx'

                    putStrLn "\n-- Heap --"
                    print heap

                    putStrLn "\n-- Evaled Equations --"
                    mapM_ print evalEqn

                    -- let (Just mainEqn) = lookup "main" equations'
                    -- putStrLn "---mainEqn---"
                    -- print mainEqn

                    putStrLn "\n-- Evaluations --"
                    printForms ctx' types evalEqn heap forms
                (Left error) -> putStrLn error


printForms :: Context -> TypeContext -> EqnContext -> Heap -> [Term] -> IO ()
printForms _ _ _ _ [] = return ()
printForms context typeContext equations heap (t:ts) = do
    putStrLn "--- HEAP ---"
    print heap
    putStrLn $ "--- " ++ (show t) ++ " ---"

    h' <- printForm context typeContext equations heap t
    printForms context typeContext equations h' ts

printForm :: Context -> TypeContext -> EqnContext -> Heap -> Term -> IO Heap
printForm context typeContext equations heap term = do
    let term' = desugarTerm term
    putStr $ showTermInContext context term'
    putStr ": "
    print $ typeOf context typeContext term'
    putStr "\n= "
    -- putStr "main = "
    let (term, h') = eval equations heap term' 
    putStrLn $ showTermInContext context $ term
    putStrLn "---\n"
    return h'

