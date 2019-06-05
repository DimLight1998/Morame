{-# LANGUAGE LambdaCase #-}
module EvalValue where

import AST
import Control.Monad.State
import Control.Monad.Zip
import qualified EvalType as T
import StackMap

type ValueMapping = StackMap String Value
type Context = ([ADT], ValueMapping)
type ContextState a = StateT Context Maybe a

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VClosure Context String Expr
  | VData String [Value]
  | VUnit -- should only be used in REPL
  deriving (Eq)

instance Show Value where
    show value = case value of
        VBool b -> if b then "true" else "false"
        VInt i -> show i
        VChar c -> show c
        VClosure {} -> "<func>"
        VData s vs -> s ++ " " ++ show vs
        VUnit -> "()"

matchPatternWithValue :: Pattern -> Value -> Maybe ValueMapping
matchPatternWithValue pat val = case pat of
    PBoolLit bool
      | VBool bool' <- val -> if bool == bool' then return [] else Nothing
    PIntLit int
      | VInt int' <- val -> if int == int' then return [] else Nothing
    PCharLit char
      | VChar char' <- val -> if char == char' then return [] else Nothing
    PVar s -> return [(s, val)]
    PData name pats
      | VData brName vs <- val, brName == name
        -> concat <$> sequence (mzipWith matchPatternWithValue pats vs)
      | VData brName vs <- val, brName /= name
        -> Nothing

getConstructorExpr :: [ADT] -> String -> Maybe Expr
getConstructorExpr adts name = let
    getConstructorValueAux :: [(String, [Type])] -> String -> Maybe Expr
    getConstructorValueAux branches name = do
        types <- lookup name branches
        let argList = map EVar $ foldr (\ elem accu -> show (length accu) : accu) [] types
        let treeWithAccu = foldr (\ tp (accu, i) -> (ELambda (show i, tp) accu, i + 1)) (EData name argList, 0) types
        return $ fst treeWithAccu
    in getConstructorValueAux (adts >>= (\ (ADT _ s) -> s)) name


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
            (adts, s) <- get
            put (adts, push (b, br) s)
            ret <- eval e
            put (adts, s)
            return ret
        ELetRec bind (arg, _) (body, _) expr -> do
            (adts, s) <- get
            -- * include this in document
            let recContext = (adts, push (bind, VClosure recContext arg body) s)
            put recContext
            ret <- eval expr
            put (adts, s)
            return ret
        EVar name -> do
            (adts, s) <- get
            -- with the support of ADT, `name` can be a constructor
            case getConstructorExpr adts name of
                Nothing -> lift $ lookUp name s
                Just e -> eval e
        EApply e1 e2 -> do
            VClosure (adts, valueMap) argName funcBody <- eval e1
            argVal <- eval e2
            backup <- get
            put (adts, push (argName, argVal) valueMap)
            ret <- eval funcBody
            put backup
            return ret
        ECase expr pes -> do
            val <- eval expr
            (adts, currentVm) <- get
            let
                evalWithPatterns :: Value -> [(Pattern, Expr)] -> ContextState Value
                evalWithPatterns val remain = case remain of
                    [] -> lift Nothing
                    (pat, expr) : remain -> case matchPatternWithValue pat val of
                        Nothing -> evalWithPatterns val remain
                        Just vm -> do
                            put (adts, reverse vm ++ currentVm)
                            ret <- eval expr
                            put (adts, currentVm)
                            return ret
            evalWithPatterns val pes
        EData name exprs -> do
            values <- mapM eval exprs
            return $ VData name values


evalProgram :: Program -> Maybe Value
evalProgram p@(Program adts body) = case T.evalType p of
    Nothing -> Nothing
    Just _ -> evalStateT (eval body) (adts, [])

evalValue :: Program -> Result
evalValue p@(Program _ expr) = case evalProgram p of
        Nothing -> RInvalid
        Just value -> case value of
            VBool b -> RBool b
            VInt i -> RInt i
            VChar c -> RChar c
            _ -> RInvalid

evalValueWithCtx :: (Context, T.Context) -> Expr -> Maybe (Value, Type)
evalValueWithCtx (ctx, tctx) expr = do
    tp <- T.evalTypeWithCtx tctx expr
    val <- evalStateT (eval expr) ctx
    return (val, tp)