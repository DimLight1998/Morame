module EvalType where

import AST
import Control.Monad.State
import Control.Monad.Zip
import Data.List
import Data.Maybe
import StackMap

type TypeMapping = StackMap String Type
type Context = ([ADT], TypeMapping)
type ContextState a = StateT Context Maybe a

matchPatternWithType :: [ADT] -> Pattern -> Type -> Maybe TypeMapping
matchPatternWithType adts pat ty = let
    matchPatternWithTypeAux :: [(String, String)] -> [(String, [Type])] -> Pattern -> Type -> Maybe TypeMapping
    matchPatternWithTypeAux brAdt flatAdts pat ty = case pat of
        PBoolLit _ -> if ty == TBool then return [] else Nothing
        PIntLit _ -> if ty == TInt then return [] else Nothing
        PCharLit _ -> if ty == TChar then return [] else Nothing
        PVar s -> return [(s, ty)]
        PData name pats -> do
            types <- lookup name flatAdts
            adt <- lookup name brAdt
            case ty of
                TData s -> 
                    if s == adt && length pats == length types
                    then concat <$> sequence (mzipWith (matchPatternWithTypeAux brAdt flatAdts) pats types)
                    else Nothing
                _ -> Nothing
    in matchPatternWithTypeAux
        (adts >>= (\ (ADT adt s) -> map (\ (br, _) -> (br, adt)) s))
        (adts >>= (\ (ADT _ s) -> s))
        pat ty

getConstructorType :: [ADT] -> String -> Maybe Type
getConstructorType adts name = let
    buildType :: String -> [Type] -> Type
    buildType adtName = foldr TArrow (TData adtName)
    getConstructorType' :: ADT -> Maybe Type
    getConstructorType' (ADT adtName branches) = case find (\ (conName, _) -> conName == name) branches of
        Nothing -> Nothing
        Just (_, types) -> Just $ buildType adtName types
    in case find (isJust . getConstructorType') adts of
        Nothing -> Nothing
        Just adt -> getConstructorType' adt

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
            (adts, s) <- get
            put (adts, push (n, tArg) s)
            tRet <- eval eBody
            put (adts, s)
            return $ TArrow tArg tRet
        ELet (n, e1) e2 -> do
            e1t <- eval e1
            (adts, s) <- get
            put (adts, push (n, e1t) s)
            e2t <- eval e2
            put (adts, s)
            return e2t
        ELetRec bind (arg, tArg) (body, tRet) expr -> do
            (adts, state) <- get
            let stateWithFunc = push (bind, TArrow tArg tRet) state
            let stateWithFuncAndArg = push (arg, tArg) stateWithFunc
            put (adts, stateWithFuncAndArg)
            tRet' <- eval body
            if tRet' == tRet
            then do
                put (adts, stateWithFunc)
                t <- eval expr
                put (adts, state)
                return t
            else do
                put (adts, state)
                lift Nothing
        EVar n -> do
            (adts, s) <- get
            -- with the support of ADT, `n` can be a constructor
            -- if it is a constructor, it will have an arrow type
            -- first check if it is a constructor, then lookup in current context
            case getConstructorType adts n of
                Nothing -> lift $ lookUp n s
                Just tp -> return tp
        EApply e1 e2 -> do
            e1t <- eval e1
            e2t <- eval e2
            case (e1t, e2t) of
                (TArrow t1 t2, t3) -> if t1 == t3 then return t2 else lift Nothing
                _ -> lift Nothing
        ECase expr pes -> do
            et <- eval expr
            (adts, currentTm) <- get
            if null pes
            then lift Nothing
            else do
                let (pats, exprs) = unzip pes
                let maybeTypeMaps = mapM (\ p -> matchPatternWithType adts p et) pats
                let
                    evalAux :: (TypeMapping, Expr) -> ContextState Type
                    evalAux (typeMap, expr) = do
                        put (adts, pushMany typeMap currentTm)
                        tp <- eval expr
                        put (adts, currentTm)
                        return tp
                case maybeTypeMaps of
                    Nothing -> lift Nothing
                    Just typeMaps -> do 
                        types <- mapM evalAux $ zip typeMaps exprs
                        if all (== head types) types then return $ head types else lift Nothing

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) (adts, [])
