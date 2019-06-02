-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State
import StackMap

type Context = StackMap String Type

type ContextState a = StateT Context Maybe a

eval :: Expr -> ContextState Type
eval expr = let
    isOfType :: Expr -> Type -> ContextState Type
    isOfType e t = do
        et <- eval e
        if et == t then return t else lift Nothing
    bothOfType :: (Expr, Expr) -> Type -> ContextState Type
    bothOfType (e1, e2) t = isOfType e1 t >> isOfType e2 t
    isEqual :: (Expr, Expr) -> ContextState Type
    isEqual (e1, e2) = do
        e1t <- eval e1
        e2t <- eval e2
        if e1t == e2t then return e1t else lift Nothing
    isEquallyFrom :: (Expr, Expr) -> [Type] -> ContextState Type
    isEquallyFrom (e1, e2) ts = do
        et <- isEqual (e1, e2)
        if et `elem` ts then return et else lift Nothing
    in case expr of
        EBoolLit _ -> return TBool
        EIntLit _ -> return TInt
        ECharLit _ -> return TChar
        ENot e -> isOfType e TBool
        EAnd e1 e2 -> bothOfType (e1, e2) TBool
        EOr e1 e2 -> bothOfType (e1, e2) TBool
        EAdd e1 e2 -> bothOfType (e1, e2) TInt
        ESub e1 e2 -> bothOfType (e1, e2) TInt
        EMul e1 e2 -> bothOfType (e1, e2) TInt
        EDiv e1 e2 -> bothOfType (e1, e2) TInt
        EMod e1 e2 -> bothOfType (e1, e2) TInt
        EEq e1 e2 -> isEquallyFrom (e1, e2) [TBool, TInt, TChar] >> return TBool
        ENeq e1 e2 -> isEquallyFrom (e1, e2) [TBool, TInt, TChar] >> return TBool
        ELt e1 e2 -> isEquallyFrom (e1, e2) [TInt, TChar] >> return TBool
        EGt e1 e2 -> isEquallyFrom (e1, e2) [TInt, TChar] >> return TBool
        ELe e1 e2 -> isEquallyFrom (e1, e2) [TInt, TChar] >> return TBool
        EGe e1 e2 -> isEquallyFrom (e1, e2) [TInt, TChar] >> return TBool
        EIf e1 e2 e3 -> isOfType e1 TBool >> isEqual (e2, e3)
        ELambda (n, tArg) eBody -> do
            s <- get
            put $ push (n, tArg) s
            tRet <- eval eBody
            put s
            return $ TArrow tArg tRet
        ELet (n, e1) e2 -> do
            e1t <- eval e1
            s <- get
            put $ push (n, e1t) s
            e2t <- eval e2
            put s
            return e2t
        ELetRec bind (arg, tArg) (body, tRet) expr -> do
            state <- get
            let stateWithFunc = push (bind, TArrow tArg tRet) state
            let stateWithFuncAndArg = push (arg, tArg) stateWithFunc
            put stateWithFuncAndArg
            tRet' <- eval body
            if tRet' == tRet
            then do
                put stateWithFunc
                t <- eval expr
                put state
                return t
            else do
                put state
                lift Nothing
        EVar n -> do
            s <- get
            lift $ lookUp n s
        EApply e1 e2 -> do
            e1t <- eval e1
            e2t <- eval e2
            case (e1t, e2t) of
                (TArrow t1 t2, t3) -> if t1 == t3 then return t2 else lift Nothing
                _ -> lift Nothing
        ECase expr pes -> undefined -- todo support ADT
        

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) []
