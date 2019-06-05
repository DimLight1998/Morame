module Main where

import AST
import Parser
import EvalValue

main :: IO ()
main = do
    putStrLn "---- Morame REPL ----"
    filePath <- getLine
    lines <- readFile filePath
    case morameParser lines of
        Nothing -> putStrLn "parse error"
        Just prog -> let res = evalValue prog in case res of
            RInvalid -> putStrLn "type check or evaluation failed"
            _ -> print res