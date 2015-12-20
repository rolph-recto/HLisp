module HLispEval (eval) where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map.Strict as M

import HLispExpr

eval :: LispEnv -> LispExpr -> LispExec
eval env expr
  -- function call
  | LispList (hd:tl) <- expr, LispSym sym <- hd = do
    apply env sym tl

  -- a list of expressions; execute one by one
  -- note that if a list is quoted it is NOT evaluated
  | LispList exprs <- expr = do
    retvals <- mapM (eval env) exprs
    return $ last retvals

  -- symbol; return binding in environment
  | LispSym sym <- expr = do
    -- check local environment first
    case M.lookup sym env of
      Just val -> return val
      -- check global environment
      Nothing -> do
        globalEnv <- lift get
        case M.lookup sym globalEnv of
          Just val -> return val
          Nothing -> throwError $ "no binding found for " ++ sym

  -- nothing to do for qlists, bools, nums, strings, etc.
  | otherwise = return expr

applyFunc :: LispEnv -> String -> [LispExpr] -> [String] -> LispExpr -> LispExec
applyFunc env fsym args params body = do
  let numArgs = length args
  let numParams = length params
  if numArgs == numParams
  then do
    evalArgs <- mapM (eval env) args
    let env' = foldr (\(key,val) acc -> M.insert key val acc) env (zip params evalArgs)
    eval env' body
  else do
    throwError $ "function " ++ fsym ++ " expects " ++ show numParams ++ " arguments, got " ++ show numArgs

apply :: LispEnv -> String -> [LispExpr] -> LispExec
apply env fsym args
  | Just (LispFunc params body) <- M.lookup fsym env = do
    applyFunc env fsym args params body
  | otherwise = do
    globalEnv <- lift get
    case M.lookup fsym globalEnv of
      -- lisp function
      Just (LispFunc params body) -> do
        applyFunc env fsym args params body

      -- primitive function defined in Haskell
      Just (LispPrimFunc n f) -> do
        let numArgs = length args
        if numArgs  == n
        then do
          -- don't evaluate arguments to primitive functions!
          -- evalArgs <- mapM (eval env) args
          f env args
        else do
          throwError $ "function " ++ fsym ++ " expects " ++ show n ++ " arguments, got " ++ show numArgs

      otherwise -> throwError $ fsym ++ " is not a function"
