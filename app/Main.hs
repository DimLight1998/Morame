module Main where

import Control.Monad.State
import Data.Maybe
import Data.List

import AST
import Parser
import qualified EvalValue as V
import qualified EvalType as T
import StackMap

type ValueContext = StackMap String V.Value
type TypeContext = StackMap String Type
type Context = ([ADT], ValueContext, TypeContext)

evalLine :: String -> StateT Context Maybe V.Value
evalLine line = do
    (adts, vm, tm) <- get
    switch (adts, vm, tm)
    where
        switch :: Context -> StateT Context Maybe V.Value
        switch (adts, vm, tm)
            | Just adt <- parseADT line = do 
                put (adt : adts, vm, tm)
                return V.VUnit
            | Just (name, expr) <- parseBinding line
            , Just (val, tp) <- V.evalValueWithCtx ((adts, vm), (adts, tm)) expr = do
                put (adts, push (name, val) vm, push (name, tp) tm)
                return V.VUnit
            | Just expr <- parseExpr line
            , Just (val, tp) <- V.evalValueWithCtx ((adts, vm), (adts, tm)) expr = return val
            | otherwise = lift Nothing

loop :: Context -> IO ()
loop ctx@(adts, vm, tm) = do
    line <- getLine
    if ":" `isPrefixOf` line
    then let
        switch :: IO ()
        switch
          | ":t" `isPrefixOf` line = do
            let expr = drop 3 line
            case parseExpr expr of
                Nothing -> print "[Error] Invalid expression." >> loop ctx
                Just expr -> case T.evalTypeWithCtx (adts, tm) expr of
                    Nothing -> print "Type checking failed." >> loop ctx
                    Just tp -> print tp >> loop ctx
          | ":env" `isPrefixOf` line = do
            print ctx
            loop ctx
          | ":q" `isPrefixOf` line = putStrLn "bye"
          | otherwise = putStrLn "[Error] Command not understood." >> loop ctx
        in switch
    else do
        let st = evalLine line
        case runStateT st ctx of
            Nothing -> putStrLn "[Error] Line ignored." >> loop ctx
            Just (val, ctx') -> putStr "=> " >> print val >> loop ctx'

main :: IO ()
main = do
    putStrLn "---- Morame REPL ----"
    loop ([], [], [])