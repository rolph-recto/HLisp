module HLisp () where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.List (intercalate)

data LispExpr =
    LispList [LispExpr]
  | LispSym String
  | LispBool Bool
  | LispNum Int
  | LispStr String
  | LispUnit -- unit value
  | LispFunc [String] [LispExpr] -- function: args, body

instance Show LispExpr where
  show (LispList l)         = "<" ++ (intercalate " " (map show l)) ++ ">"
  show (LispSym s)          = s
  show (LispBool b)         = show b
  show (LispNum n)          = show n
  show (LispStr s)          = "\"" ++ s ++ "\""
  show LispUnit             = "()"
  show (LispFunc args body) =
    "<\\" ++ (intercalate " " args) ++ " -> " ++ (intercalate " " (map show body)) ++ ">"

type LispEnv = M.Map String LispExpr

-- primitive functions and number of parameters they take
primitives = [("print",1),("set",2),("fun",2),("if",3)]
  
eval :: LispEnv -> LispExpr -> ExceptT String (StateT LispEnv IO) LispExpr
eval env expr
  -- function call
  | LispList (hd:tl) <- expr, LispSym sym <- hd = apply env sym tl
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
  | otherwise = return expr

apply :: LispEnv -> String -> [LispExpr] -> ExceptT String (StateT LispEnv IO) LispExpr
apply env fsym args
  | Just numParams <- fsym `lookup` primitives = do
    let numArgs = length args
    if numParams == numArgs
    then evalPrimitive env fsym args
    else throwError $ fsym ++ " expression expects " ++ show numParams ++ " arguments, got " ++ show numArgs
  | otherwise = do
    case M.lookup fsym env of
      Just (LispFunc params body) -> do
        let numArgs = length args
        let numParams = length params
        if numArgs == numParams
        then do
          evalArgs <- mapM (eval env) args
          let env' = foldr (\(key,val) acc -> M.insert key val acc) env (zip params evalArgs)
          retvals <- mapM (eval env') body
          return $ last retvals -- function only returns last value
        else do
          throwError $ "function " ++ fsym ++ " expects " ++ show numParams ++ " arguments, got " ++ show numArgs

      otherwise -> throwError $ fsym ++ " is not a function"
  
evalPrimitive :: LispEnv -> String -> [LispExpr] -> ExceptT String (StateT LispEnv IO) LispExpr
evalPrimitive env fsym args
  -- print something out
  | fsym == "print" = do
    forM_ args $ \arg -> do
      case arg of
        LispStr s -> liftIO $ putStrLn s
        _         -> liftIO $ putStrLn $ show arg

    return LispUnit

  -- add binding to global environment
  | fsym == "set" = do
    case args !! 0 of
      LispSym s -> do
        globalEnv <- lift get
        val <- eval env (args !! 1)
        lift $ put $ M.insert s val globalEnv
        return LispUnit
      otherwise -> throwError "first argument to set must be a symbol"

  -- convert list to function
  | fsym == "fun" = do
    case args of
      [LispList params, LispList body] -> do
        if all isSym params
        then return $ LispFunc (map symStr params) body
        else throwError "params must be symbols"

      _ -> throwError "first and second args to fun must be lists"

  -- conditional expression
  | fsym == "if" = do
    predVal <- eval env (args !! 0)
    case predVal of 
      LispBool True -> eval env (args !! 1)
      LispBool False -> eval env (args !! 2)
      otherwise -> throwError "if predicate must be boolean"

  where isSym (LispSym _)   = True
        isSym _             = False
        symStr (LispSym s)  = s


main = do
  let thenExpr = LispList [LispSym "print", LispStr "Hello world"]
  let elseExpr = LispList [LispSym "print", LispStr "Goodbye world"]
  let expr = LispList [LispSym "if", LispBool True, thenExpr, elseExpr]
  result <- evalStateT (runExceptT $ eval M.empty expr) M.empty
  case result of
    Left err -> putStrLn err
    Right LispUnit -> return ()
    Right val      -> putStrLn $ show val

