module Language.HLisp.Eval (
  eval,
  apply, applyFunc, applyPrimFunc,
  runLisp
) where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map.Strict as M

import Language.HLisp.Expr

-- replace symbols in an expr
subSymbol :: String -> String -> LispExpr a -> LispExpr a
subSymbol old new expr
  | LispList exprs <- expr = LispList (map (subSymbol old new) exprs)
  | LispQList exprs <- expr = LispQList (map (subSymbol old new) exprs)
  | LispSym s <- expr = if old == s then LispSym new else LispSym s
  | LispFunc env params body <- expr =
    if old `elem` params then expr else LispFunc env params (subSymbol old new body)
  | otherwise = expr

-- append a number to symbol to create a new one
renameParam :: LispEnv a -> Int -> String -> String
renameParam env i param =
  if newParam `M.member` env then renameParam env (i+1) param else newParam
  where newParam = param ++ (show i)

-- rename bound variables in bound to prevent
-- substitution conflicts
-- returns new params (subbed or not) and expr with subbed symbols
alphaRename :: LispEnv a -> [String] -> LispExpr a -> ([String], LispExpr a)
alphaRename env params expr = (params', expr')
  where paramsToRename = map (\p -> (p, p `M.member` env)) params
        shouldRename (p,rename) = if rename then renameParam env 0 p else p
        params' = map shouldRename paramsToRename
        subbedParams = filter (\((_,rename),_) -> rename) $ zip paramsToRename params'
        paramSub = map (\((old,_),new) -> (old,new)) subbedParams
        expr' = foldr (\sub acc -> uncurry subSymbol sub acc) expr paramSub

eval :: LispEnv a -> LispExpr a -> LispExec a
eval env expr
  -- function call
  | LispList (hd:tl) <- expr, LispSym sym <- hd = do
    apply env sym tl

  -- a list of expressions; execute one by one
  -- note that if a list is quoted it is NOT evaluated
  | LispList exprs <- expr = do
    retvals <- mapM (eval env) exprs
    return $ last retvals

  -- don't evaluate anything in a qlist
  | LispQList exprs <- expr = return expr

  -- symbol; return binding in environment
  | LispSym sym <- expr = do
    -- check local environment first
    case M.lookup sym env of
      Just val -> return val
      -- check global environment
      Nothing -> do
        (_,globalEnv) <- get
        case M.lookup sym globalEnv of
          Just val -> return val
          Nothing -> throwError $ "no binding found for " ++ sym

  -- nothing to do for qlists, bools, nums, strings, etc.
  | otherwise = return expr

apply :: LispEnv a -> String -> [LispExpr a] -> LispExec a
apply env fsym args = do
  globalEnv <- getLispState
  apply_ env fsym args [env,globalEnv]
  where apply_ env fsym args [] = throwError $ notAFunction fsym
        apply_ env fsym args (henv:tlenv) =do
          case M.lookup fsym henv of
            Just (LispFunc closure_env params body) -> do
              -- restore closure env 
              let env' = restoreClosure closure_env env
              applyFunc env' fsym args params body

            Just (LispLFunc closure_env params body) -> do
              -- restore closure env 
              let env' = restoreClosure closure_env env
              applyLFunc env' fsym args params body

            Just (LispPrimFunc n f) -> do
              applyPrimFunc env fsym args n f

            -- an unevaluated function
            Just expr@(LispList _) -> do
              fexpr <- eval env expr
              case fexpr of
                LispFunc _ _ _ -> do
                  apply (M.insert fsym fexpr env) fsym args

                LispLFunc _ _ _ -> do
                  apply (M.insert fsym fexpr env) fsym args

                LispPrimFunc _ _ -> do
                  apply (M.insert fsym fexpr env) fsym args

                otherwise -> do
                  throwError $ notAFunction fsym 

            otherwise -> apply_ env fsym args tlenv

        restoreClosure closure_env env =
          foldr (uncurry M.insert) env (M.toList closure_env)

        notAFunction fsym = fsym ++ " is not a function"

applyFunc :: LispEnv a -> String -> [LispExpr a] -> [String] -> LispExpr a -> LispExec a
applyFunc env fsym args params body = do
  let numArgs = length args
  let numParams = length params
  let (params',body') = alphaRename env params body
  evalArgs <- mapM (eval env) args
  let env' = foldr (uncurry M.insert) env (zip params' evalArgs)
  if numArgs == numParams
  then do
    -- full application; evaluate body
    eval env' body'
  else do
    -- partial application; return a function
    if numArgs < numParams
    then do
      return $ LispFunc env' (drop numArgs params') body'
    else do
      throwError $ "function " ++ fsym ++ " expects " ++ show numParams ++ " arguments, got " ++ show numArgs

-- this evaluates lazy functions (lfuncs), which are similar to fexprs
-- the easy way to making a function lazy is to convert all of the
-- unevaluated lists to quoted lists; that way they will not be evaluated
-- unless it is passed as an argument to the 'eval' primitive
applyLFunc :: LispEnv a -> String -> [LispExpr a] -> [String] -> LispExpr a -> LispExec a
applyLFunc env fsym args params body = do
  let largs = map makeLazyArg args
  applyFunc env fsym largs params body
  where makeLazyArg  (LispList l) = LispQList l
        makeLazyArg p = p

applyPrimFunc :: LispEnv a -> String -> [LispExpr a] -> Int -> PrimFunc a -> LispExec a
applyPrimFunc env fsym args n f = do
  let numArgs = length args
  if numArgs == n || n < 0
  then do
    -- don't evaluate arguments to primitive functions!
    -- evalArgs <- mapM (eval env) args
    -- we don't need to alpha rename parameters either, since
    -- primitive functions use only the position of arguments
    -- to distinguish them
    f env args
  else do
    throwError $ "primitive function " ++ fsym ++ " expects " ++ show n ++ " arguments, got " ++ show numArgs

runLisp :: LispState a -> LispExpr a -> IO (Either String (LispExpr a), LispState a)
runLisp state expr = runStateT (runExceptT $ eval M.empty expr) state
