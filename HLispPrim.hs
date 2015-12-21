module HLispPrim (primitives) where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map.Strict as M

import HLispExpr
import HLispEval

-- print something out
printPrimitive :: LispEnv -> [LispExpr] -> LispExec
printPrimitive env (arg:_) = do
  val <- eval env arg
  case val of
    LispStr s -> liftIO $ putStrLn s
    _         -> liftIO $ putStrLn $ show val

  return LispUnit

-- get user input from stdin
inputPrimitive :: LispEnv -> [LispExpr] -> LispExec
inputPrimitive env args = do
  case args of
    [] -> do
      line <- liftIO getLine
      return $ LispStr line

    otherwise -> throwError "input takes no arguments!"
  
-- set a binding in the global environment
setPrimitive :: LispEnv -> [LispExpr] -> LispExec
setPrimitive env args = do
  case args !! 0 of
    LispSym s -> do
      globalEnv <- lift get
      val <- eval env (args !! 1)
      lift $ put $ M.insert s val globalEnv
      return LispUnit
    otherwise -> throwError "first argument to set must be a symbol"

-- convert list to function
funPrimitive :: LispEnv -> [LispExpr] -> LispExec
funPrimitive env args = do
  case args of
    [LispList params, body@(LispList _)] -> do
      if all isSym params
      then return $ LispFunc (map symStr params) body
      else throwError "params must be symbols"

    _ -> throwError "first and second args to fun must be lists"

  where isSym (LispSym _)   = True
        isSym _             = False
        symStr (LispSym s)  = s

-- conditional expression
-- it's important that this has lazy semantics because branches could
-- have side effects so we shouldn't execute a branch until we know it's
-- the right one!
ifPrimitive :: LispEnv -> [LispExpr] -> LispExec
ifPrimitive env args = do
  predVal <- eval env (args !! 0)
  case predVal of 
    LispBool True -> eval env (args !! 1)
    LispBool False -> eval env (args !! 2)
    otherwise -> throwError "if predicate must be boolean"

-- let expression
-- this is basically a function application that can set the
-- name of its parameter
letPrimitive :: LispEnv -> [LispExpr] -> LispExec
letPrimitive env args = do
  let (body:letbinds) = reverse args 
  binds <- forM (reverse letbinds) $ \letbind -> do
    case letbind of
      LispList [LispSym var, val] -> return (var,val)
      otherwise -> do
        throwError "let binds must have form [let [var val] ... [var val] body]"

  let (vars, vals) = unzip binds
  applyFunc env "let" vals vars body
  
-- arithmetic primitives
addPrimitive :: LispEnv -> [LispExpr] -> LispExec
addPrimitive env (x:y:_) = do
  [xval,yval] <- mapM (eval env) [x,y]
  case (xval,yval) of
    (LispNum xint, LispNum yint)  -> return $ LispNum (xint + yint)
    _                     -> throwError "+ takes two integers" 

subPrimitive :: LispEnv -> [LispExpr] -> LispExec
subPrimitive env (x:y:_) = do
  [xval,yval] <- mapM (eval env) [x,y]
  case (xval,yval) of
    (LispNum xint, LispNum yint)  -> return $ LispNum (xint - yint)
    _                     -> throwError "- takes two integers" 

mulPrimitive :: LispEnv -> [LispExpr] -> LispExec
mulPrimitive env (x:y:_) = do
  [xval,yval] <- mapM (eval env) [x,y]
  case (xval,yval) of
    (LispNum xint, LispNum yint)  -> return $ LispNum (xint * yint)
    _                     -> throwError "* takes two integers" 


divPrimitive :: LispEnv -> [LispExpr] -> LispExec
divPrimitive env (x:y:_) = do
  [xval,yval] <- mapM (eval env) [x,y]
  case (xval,yval) of
    (LispNum xint, LispNum 0)     -> throwError "division by zero"
    (LispNum xint, LispNum yint)  -> return $ LispNum (div xint yint)
    _                             -> throwError "* takes two integers" 

arithBoolPrimitive :: (Int -> Int-> Bool) -> String -> LispEnv -> [LispExpr] -> LispExec
arithBoolPrimitive f fsym env (x:y:_) = do
  [xval, yval] <- mapM (eval env) [x, y]
  case (xval,yval) of
    (LispNum xint, LispNum yint) -> return $ LispBool (f xint yint)
    _                         -> throwError $ fsym ++ " takes two integers"

ltPrimitive = arithBoolPrimitive (<) "<"
gtPrimitive = arithBoolPrimitive (>) ">"
eqPrimitive = arithBoolPrimitive (==) "=="

-- list primitives
headPrimitive :: LispEnv -> [LispExpr] -> LispExec
headPrimitive env (lst:_) = do
  lstval <- eval env lst
  case lstval of
    LispQList (x:_)   -> eval env x
    LispQList []      -> throwError "head expects non-empty list"
    otherwise         -> throwError "head expects list argument"

tailPrimitive :: LispEnv -> [LispExpr] -> LispExec
tailPrimitive env (lst:_) = do
  lstval <- eval env lst
  case lstval of
    LispQList (_:xs)  -> eval env $ LispQList xs
    otherwise         -> throwError "tail expects list argument"

nilPrimitive :: LispEnv -> [LispExpr] -> LispExec
nilPrimitive env (lst:_) = do
  lstval <- eval env lst
  case lstval of
    LispQList (_:_)   -> return $ LispBool False
    LispQList []      -> return $ LispBool True
    otherwise         -> throwError "nil? expects list argument"

consPrimitive :: LispEnv -> [LispExpr] -> LispExec
consPrimitive env (hd:tl:_) = do
  hdval <- eval env hd
  tlval <- eval env tl
  case tlval of
    LispQList xs      -> return $ LispQList (hdval:xs)
    otherwise         -> throwError "cons expects 2nd argument to be a list"

-- concatenate some strings together
concatPrimitive :: LispEnv -> [LispExpr] -> LispExec
concatPrimitive env args = do
  evalArgs <- mapM (eval env) args
  strs <- forM evalArgs $ \evalArg -> do
    case evalArg of
      LispStr s -> return s
      otherwise -> throwError "concat takes strings as arguments"

  return $ LispStr $ foldr (++) "" strs

-- primitive functions and number of parameters they take
primitives :: [(String, (Int,PrimFunc))]
primitives = [
  -- control / io primitives
  ("print",(1,printPrimitive)),
  ("input",(0,inputPrimitive)),
  ("set",(2,setPrimitive)),
  ("fun",(2,funPrimitive)),
  ("if",(3,ifPrimitive)),
  ("let",(-1,letPrimitive)), -- let has a variable number of arguments
  -- arith + bool primitives
  ("+",(2,addPrimitive)),
  ("-",(2,subPrimitive)),
  ("*",(2,mulPrimitive)),
  ("/",(2,divPrimitive)),
  ("<",(2,ltPrimitive)),
  (">",(2,gtPrimitive)),
  ("==",(2,eqPrimitive)),
  -- list primitives
  ("head",(1,headPrimitive)),
  ("tail",(1,tailPrimitive)),
  ("nil?",(1,nilPrimitive)),
  ("cons",(2,consPrimitive)),
  -- string primitives
  ("concat",(-1,concatPrimitive))]
