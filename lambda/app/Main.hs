module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main = print $ parseUntypedLambda "x x"

-- main :: IO ()
-- main = do
--     args <- getArgs
--     case args of
--         [fileName] -> runFile fileName
--         _ -> putStrLn "Usage: untyped-arith-exe <file-name>"


-- runFile :: String -> IO ()
-- runFile fileName = do
--     contents <- readFile fileName
--     let forms = parseUntypedArith contents
--     case forms of
--         (Right forms) -> mapM_ (print . eval) forms
--         _ -> putStrLn "Error in file."