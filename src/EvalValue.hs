{-# LANGUAGE LambdaCase #-}
module EvalValue where

import AST
import Control.Monad.State
import qualified EvalType as T
import StackMap

type Context = StackMap String Value

type ContextState a = StateT Context Maybe a

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VClosure Context String Expr
  deriving (Show, Eq)

eval :: Expr -> ContextState Value
eval expr = let
    binaryIntOp :: Expr -> Expr -> (Int -> Int -> Int) -> ContextState Value
    binaryIntOp e1 e2 op = do
        VInt i1 <- eval e1
        VInt i2 <- eval e2
        return $ VInt $ i1 `op` i2
    binaryEqOp :: Expr -> Expr -> (Value -> Value -> Bool) -> ContextState Value
    binaryEqOp e1 e2 op = do
        v1 <- eval e1
        v2 <- eval e2
        return $ VBool $ v1 `op` v2
    binaryCmpOp :: Expr -> Expr -> (Int -> Int -> Bool, Char -> Char -> Bool) -> ContextState Value
    binaryCmpOp e1 e2 (opi, opc) = do
        v1 <- eval e1
        v2 <- eval e2
        case (v1, v2) of
            (VInt i1, VInt i2) -> return $ VBool $ i1 `opi` i2
            (VChar c1, VChar c2) -> return $ VBool $ c1 `opc` c2
    in case expr of
        EBoolLit b -> return $ VBool b
        EIntLit i -> return $ VInt i
        ECharLit c -> return $ VChar c
        ENot e -> eval e >>= \ case
            VBool b -> return $ VBool $ not b
            _ -> lift Nothing
        EAnd e1 e2 -> eval $ EIf e1 e2 (EBoolLit False)
        EOr e1 e2 -> eval $ EIf e1 (EBoolLit True) e2
        EAdd e1 e2 -> binaryIntOp e1 e2 (+)
        ESub e1 e2 -> binaryIntOp e1 e2 (-)
        EMul e1 e2 -> binaryIntOp e1 e2 (*)
        EDiv e1 e2 -> binaryIntOp e1 e2 div
        EMod e1 e2 -> binaryIntOp e1 e2 mod
        EEq e1 e2 -> binaryEqOp e1 e2 (==)
        ENeq e1 e2 -> binaryEqOp e1 e2 (/=)
        ELt e1 e2 -> binaryCmpOp e1 e2 ((<), (<))
        EGt e1 e2 -> binaryCmpOp e1 e2 ((>), (>))
        ELe e1 e2 -> binaryCmpOp e1 e2 ((<=), (<=))
        EGe e1 e2 -> binaryCmpOp e1 e2 ((>=), (>=))
        EIf e1 e2 e3 -> do
            VBool c <- eval e1
            if c then eval e2 else eval e3
        ELambda (arg, _) e -> do
            s <- get
            return $ VClosure s arg e
        ELet (b, be) e -> do
            br <- eval be
            s <- get
            put $ push (b, br) s
            ret <- eval e
            put s
            return ret
        ELetRec bind (arg, _) (body, _) expr -> do
            s <- get
            let recContext = push (bind, VClosure recContext arg body) s
            put recContext
            ret <- eval expr
            put s
            return ret
        EVar name -> do
            s <- get
            lift $ lookUp name s
        EApply e1 e2 -> do
            VClosure context argName funcBody <- eval e1
            argVal <- eval e2
            backup <- get
            put $ push (argName, argVal) context
            ret <- eval funcBody
            put backup
            return ret
        ECase _ _ -> undefined -- todo support ADT

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) []

evalValue :: Program -> Result
evalValue p@(Program _ expr) = case T.evalType p of
    Nothing -> RInvalid
    Just _ -> case evalProgram p of
        Nothing -> RInvalid
        Just value -> case value of
            VBool b -> RBool b
            VInt i -> RInt i
            VChar c -> RChar c
            _ -> RInvalid
