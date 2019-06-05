module Main where

import Control.Monad.State
import Data.Maybe

import AST
import Parser
import EvalValue hiding (Context)
import StackMap

type ValueContext = StackMap String Value
type TypeContext = StackMap String Type
type Context = ([ADT], ValueContext, TypeContext)

evalLine :: String -> StateT Context Maybe Value
evalLine line = do
    (adts, vm, tm) <- get
    switch (adts, vm, tm)
    where
        switch :: Context -> StateT Context Maybe Value
        switch (adts, vm, tm)
            | Just adt <- parseADT line = do 
                put (adt : adts, vm, tm)
                return VUnit
            | Just (name, expr) <- parseBinding line
            , Just (val, tp) <- evalValueWithCtx ((adts, vm), (adts, tm)) expr = do
                put (adts, push (name, val) vm, push (name, tp) tm)
                return VUnit
            | Just expr <- parseExpr line
            , Just (val, tp) <- evalValueWithCtx ((adts, vm), (adts, tm)) expr = return val
            | otherwise = lift Nothing

loop :: Context -> IO ()
loop ctx = do
    line <- getLine
    let st = evalLine line
    case runStateT st ctx of
        Nothing -> putStrLn "[Error] Line ignored." >> loop ctx
        Just (val, ctx') -> putStr "=>  " >> print val >> loop ctx'

main :: IO ()
main = do
    putStrLn "---- Morame REPL ----"
    loop ([], [], [])