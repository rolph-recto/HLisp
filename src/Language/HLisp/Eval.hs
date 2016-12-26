module Language.HLisp.Eval (
  eval,
  apply, applyFunc, applyPrimFunc,
  runLisp
) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Language.HLisp.Expr

-- replace symbols in an expr
subSymbol :: String -> String -> LispExpr a -> LispExpr a
subSymbol old new expr
  | LispList exprs <- expr = LispList (map (subSymbol old new) exprs)
  | LispQList exprs <- expr = LispQList (map (subSymbol old new) exprs)
  | LispSym s <- expr = if old == s then LispSym new else LispSym s
  | LispFunc globalEnv env params body <- expr =
    if old `elem` params then expr else LispFunc globalEnv env params (subSymbol old new body)
  | LispLFunc globalEnv env params body <- expr =
    if old `elem` params then expr else LispLFunc globalEnv env params (subSymbol old new body)
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

-- any expr except a list is a value!
isValue :: LispExpr a -> Bool
isValue expr = case expr of
  LispList _ -> False
  _ -> True

eval :: LispEnv a -> LispExpr a -> LispExec a
eval env expr
  -- function call
  | LispList (hd:tl) <- expr, LispSym sym <- hd = do
    apply env sym tl

  -- a list of expressions; execute one by one
  -- note that if a list is quoted it is NOT evaluated
  | LispList (cmd:tlcmds) <- expr = do
    pushLispStack $ const (LispList tlcmds)
    eval env cmd

  | LispList [] <- expr = retEval env LispUnit

  -- don't evaluate anything in a qlist
  | LispQList exprs <- expr = retEval env expr

  -- symbol; return binding in environment
  | LispSym sym <- expr = do
    -- check local environment first
    case M.lookup sym env of
      Just val -> retEval env val
      -- check global environment
      Nothing -> do
        globalEnv <- getLispState
        case M.lookup sym globalEnv of
          Just val -> retEval env val
          Nothing -> throwError $ "no binding found for " ++ sym

  -- nothing to do for qlists, bools, nums, strings, etc.
  | otherwise = retEval env expr

apply :: LispEnv a -> String -> [LispExpr a] -> LispExec a
apply env fsym args = do
  globalEnv <- getLispState
  apply_ env fsym args [env,globalEnv]
  where apply_ env fsym args [] = throwError $ notAFunction fsym
        apply_ env fsym args (henv:tlenv) =do
          case M.lookup fsym henv of
            Just (LispFunc cloGlobalEnv cloLocalEnv params body) -> do
              -- restore closure env 
              let env' = restoreClosure cloLocalEnv env
              applyFunc cloGlobalEnv env' fsym args params body

            Just (LispLFunc cloGlobalEnv cloLocalEnv params body) -> do
              -- restore closure env 
              let env' = restoreClosure cloLocalEnv env
              applyLFunc cloGlobalEnv env' fsym args params body

            Just (LispPrimFunc n f) -> do
              applyPrimFunc env fsym args n f

            -- an unevaluated function
            Just expr@(LispList _) -> do
              fexpr <- eval env expr
              case fexpr of
                LispFunc _ _ _ _ -> do
                  apply (M.insert fsym fexpr env) fsym args

                LispLFunc _ _ _ _ -> do
                  apply (M.insert fsym fexpr env) fsym args

                LispPrimFunc _ _ -> do
                  apply (M.insert fsym fexpr env) fsym args

                otherwise -> do
                  throwError $ notAFunction fsym 

            otherwise -> apply_ env fsym args tlenv

        restoreClosure closure_env env =
          foldr (uncurry M.insert) env (M.toList closure_env)

        notAFunction fsym = fsym ++ " is not a function"

applyFunc :: LispEnv a -> LispEnv a -> String -> [LispExpr a] -> [String] -> LispExpr a -> LispExec a
applyFunc cloGlobalEnv env fsym args params body
  -- all parameters reduced to values; evaluate function
  | all isValue args = do
    let numArgs = length args
    let numParams = length params
    let (params',body') = alphaRename env params body
    let env' = foldr (uncurry M.insert) env (zip params' args)
    if numArgs == numParams
    then do
        -- full application; evaluate body
        globalEnv <- getLispState
        let globalEnv' = M.union cloGlobalEnv globalEnv
        putLispState globalEnv'
        eval env' body'
    else do
        -- partial application; return a function
        if numArgs < numParams
        then do
          retEval env $ LispFunc cloGlobalEnv env' (drop numArgs params') body'
        else do
          let msg f np na = "function " ++ f ++ " expects " ++ np ++ " arguments, got " ++ na
          throwError $ msg fsym (show numParams) (show numArgs)

  -- evaluate arguments
  | otherwise = do
    let (evalArgs, arg:restArgs) = L.partition isValue args
    let cont x = LispList ((LispSym fsym:evalArgs) ++ (x:restArgs))
    pushLispStack cont
    eval env arg

-- this evaluates lazy functions (lfuncs), which are similar to fexprs
-- the easy way to making a function lazy is to convert all of the
-- unevaluated lists to quoted lists; that way they will not be evaluated
-- unless it is passed as an argument to the 'eval' primitive
applyLFunc :: LispEnv a -> LispEnv a -> String -> [LispExpr a] -> [String] -> LispExpr a -> LispExec a
applyLFunc globalEnv env fsym args params body = do
  let largs = map makeLazyArg args
  applyFunc globalEnv env fsym largs params body
  where makeLazyArg  (LispList l) = LispQList l
        makeLazyArg p = p

applyPrimFunc :: LispEnv a -> String -> [LispExpr a] -> Int -> PrimFunc a -> LispExec a
applyPrimFunc env fsym args n f = do
  liftIO $ print "PRIM FUNC SYM"
  liftIO $ print fsym
  liftIO $ print "PRIM FUNC ARGS"
  liftIO $ print args
  let numArgs = length args
  if numArgs == n || n < 0
  then do
    -- don't evaluate arguments to primitive functions!
    -- evalArgs <- mapM (eval env) args
    -- we don't need to alpha rename parameters either, since
    -- primitive functions use only the position of arguments
    -- to distinguish them
    primret <- f env args
    liftIO $ print "PRIMRET"
    liftIO $ print primret
    retEval env primret
  else do
    let msg f np na = "primitive function " ++ f ++ " expects " ++ np ++ " arguments, got " ++ na
    throwError $ msg fsym (show n) (show numArgs)

retEval :: LispEnv a -> LispExpr a -> LispExec a
retEval env expr = do
  st <- getLispStack
  case st of
    [] -> return expr
    (c:cs) -> do
      putLispStack cs
      eval env (c expr) 

runLisp :: LispState a -> LispExpr a -> IO (Either String (LispExpr a), LispState a)
runLisp state expr = runStateT (runExceptT (eval M.empty expr)) state
