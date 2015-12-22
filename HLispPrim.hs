module HLispPrim (primitives) where

import System.Random

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map.Strict as M
import Text.Read (readMaybe)
import Data.Char (toLower)

import HLispExpr
import HLispEval

-- print something out
printPrimitive :: PrimFunc a
printPrimitive env args = do
  vals <- mapM (eval env) args
  forM vals $ \val ->
    case val of
      LispStr s -> liftIO $ putStr s
      _         -> liftIO $ putStr (show val)

  liftIO $ putStrLn ""

  return LispUnit

-- get user input from stdin
inputPrimitive :: PrimFunc a
inputPrimitive env args = do
  line <- liftIO getLine
  return $ LispStr line

inputNumPrimitive :: PrimFunc a
inputNumPrimitive env args = do
  line <- liftIO getLine
  case readMaybe line of
    Just n -> return $ LispNum n
    Nothing -> throwError "expected num input!"

inputBoolPrimitive :: PrimFunc a
inputBoolPrimitive env args = do
  line <- liftIO getLine
  case map toLower line of
    "true"  -> return $ LispBool True
    "false" -> return $ LispBool False
    _       -> throwError "input: expected bool"
  
  
-- set a binding in the global environment
setPrimitive :: PrimFunc a
setPrimitive env args = do
  case args !! 0 of
    LispSym s -> do
      (userState, globalEnv) <- lift get
      val <- eval env (args !! 1)
      lift $ put $ (userState, M.insert s val globalEnv)
      return LispUnit

    otherwise -> throwError "set: first argument must be a symbol"

-- convert list to function
funPrimitive :: PrimFunc a
funPrimitive env args = do
  case args of
    [LispList params, body@(LispList _)] -> do
      if all isSym params
      then return $ LispFunc (map symStr params) body
      else throwError "fun: params must be symbols"

    _ -> throwError "fun: first and second arguments must be lists"

  where isSym (LispSym _)   = True
        isSym _             = False
        symStr (LispSym s)  = s

-- conditional expression
-- it's important that this has lazy semantics because branches could
-- have side effects so we shouldn't execute a branch until we know it's
-- the right one!
ifPrimitive :: PrimFunc a
ifPrimitive env args = do
  predVal <- eval env (args !! 0)
  case predVal of 
    LispBool True -> eval env (args !! 1)
    LispBool False -> eval env (args !! 2)
    otherwise -> throwError "if: predicate must be boolean"

whilePrimitive :: PrimFunc a
whilePrimitive env (pred:body:_) = do
  predval <- eval env pred
  case predval of
    LispBool True   -> do
      eval env body
      whilePrimitive env [pred,body]
    LispBool False  -> return LispUnit
    otherwise       -> throwError "while: guard must be a boolean"

forPrimitive :: PrimFunc a
forPrimitive env (var:lst:body:_) = do
  lstval <- eval env lst
  case (var,lstval) of
    (LispSym s, LispQList iterexprs) -> do
      forM_ iterexprs $ \iterexpr -> do
        iterval <- eval env iterexpr
        let env' = M.insert s iterval env
        eval env' body

      return LispUnit

    otherwise -> throwError "for: arguments must be symbol and quoted list"

repeatPrimitive :: PrimFunc a
repeatPrimitive env (n':body:_) = do
  nval <- eval env n'
  case nval of
    LispNum n -> do
      forM_ [1..n] $ \_ -> (eval env body)
      return LispUnit

    otherwise -> throwError "repeat: first argument must be a num"
  
  
-- let expression
-- this is basically a function application that can set the
-- name of its parameter
letPrimitive :: PrimFunc a
letPrimitive env args = do
  let (body:letbinds) = reverse args 
  binds <- forM (reverse letbinds) $ \letbind -> do
    case letbind of
      LispList [LispSym var, val] -> return (var,val)
      otherwise -> do
        throwError "let: binds must have form [let [var val] ... [var val] body]"

  let (vars, vals) = unzip binds
  applyFunc env "let" vals vars body

-- random number generator
-- returns an int between lo and hi
randomPrimitive :: PrimFunc a
randomPrimitive env (lo':hi':_) = do
  loVal <- eval env lo'
  hiVal <- eval env hi'
  case (loVal,hiVal) of
    (LispNum lo, LispNum hi) -> do
      if lo >= hi
      then do
        throwError "random: first argument must be less than second argument"

      else do
        val <- liftIO $ getStdRandom (randomR (lo,hi))
        return $ LispNum val

    _ -> throwError "random: expected numbers as arguments"
  
-- arithmetic primitives
addPrimitive :: PrimFunc a
addPrimitive env (x:y:_) = do
  [xval,yval] <- mapM (eval env) [x,y]
  case (xval,yval) of
    (LispNum xint, LispNum yint)  -> return $ LispNum (xint + yint)
    _                     -> throwError "+ takes two integers" 

subPrimitive :: PrimFunc a
subPrimitive env (x:y:_) = do
  [xval,yval] <- mapM (eval env) [x,y]
  case (xval,yval) of
    (LispNum xint, LispNum yint)  -> return $ LispNum (xint - yint)
    _                     -> throwError "- takes two integers" 

mulPrimitive :: PrimFunc a
mulPrimitive env (x:y:_) = do
  [xval,yval] <- mapM (eval env) [x,y]
  case (xval,yval) of
    (LispNum xint, LispNum yint)  -> return $ LispNum (xint * yint)
    _                     -> throwError "* takes two integers" 


divPrimitive :: PrimFunc a
divPrimitive env (x:y:_) = do
  [xval,yval] <- mapM (eval env) [x,y]
  case (xval,yval) of
    (LispNum xint, LispNum 0)     -> throwError "division by zero"
    (LispNum xint, LispNum yint)  -> return $ LispNum (div xint yint)
    _                             -> throwError "* takes two integers" 

arithBoolPrimitive :: (Int -> Int-> Bool) -> String -> PrimFunc a
arithBoolPrimitive f fsym env (x:y:_) = do
  [xval, yval] <- mapM (eval env) [x, y]
  case (xval,yval) of
    (LispNum xint, LispNum yint) -> return $ LispBool (f xint yint)
    _                         -> throwError $ fsym ++ " takes two integers"

ltPrimitive = arithBoolPrimitive (<) "<"
gtPrimitive = arithBoolPrimitive (>) ">"

-- eqPrimitive can compare nums, strings and bools
eqPrimitive :: PrimFunc a
eqPrimitive env (x:y:_) = do
  [xval,yval] <- mapM (eval env) [x,y]
  case (xval,yval) of
    (LispNum xint, LispNum yint) -> return $ LispBool (xint == yint)
    (LispBool xbool, LispBool ybool) -> return $ LispBool (xbool == ybool)
    (LispStr xstr, LispStr ystr) -> return $ LispBool (xstr == ystr)
    otherwise -> throwError "== takes either two nums, two strings or two bools"

-- list and string primitives
headPrimitive :: PrimFunc a
headPrimitive env (lst:_) = do
  lstval <- eval env lst
  case lstval of
    LispQList (x:_)   -> eval env x
    LispQList []      -> throwError "head: expected non-empty list"
    otherwise         -> throwError "head: expected list argument"

tailPrimitive :: PrimFunc a
tailPrimitive env (lst:_) = do
  lstval <- eval env lst
  case lstval of
    LispQList (_:xs)  -> eval env $ LispQList xs
    otherwise         -> throwError "tail: expected list argument"

nilPrimitive :: PrimFunc a
nilPrimitive env (lst:_) = do
  lstval <- eval env lst
  case lstval of
    LispQList l       -> return $ LispBool $ length l == 0
    LispStr s         -> return $ LispBool $ length s == 0
    otherwise         -> throwError "nil?: expected list argument"

consPrimitive :: PrimFunc a
consPrimitive env (hd:tl:_) = do
  hdval <- eval env hd
  tlval <- eval env tl
  case tlval of
    LispQList xs      -> return $ LispQList (hdval:xs)
    otherwise         -> throwError "cons: expected second argument to be a list"

-- access an element of a list/string
indexPrimitive :: PrimFunc a
indexPrimitive env (index:lst:_) = do
  evalArgs <- mapM (eval env) [index,lst]
  case evalArgs of
    [LispQList qlst, LispNum n] -> do
      if n < length qlst
      then return $ qlst !! n
      else throwError "list index out of bounds"
    [LispStr str, LispNum n] -> do
      if n < length str
      then return $ LispStr [(str !! n)]
      else throwError "list index out of bounds"
    otherwise -> throwError "!: expected list/string and num as arguments"

-- concatenate some strings together
concatPrimitive :: PrimFunc a
concatPrimitive env args = do
  evalArgs <- mapM (eval env) args
  strs <- forM evalArgs $ \evalArg -> do
    case evalArg of
      LispStr s -> return s
      otherwise -> throwError "concat: expected strings as arguments"

  return $ LispStr $ foldr (++) "" strs

-- primitive functions and number of parameters they take
primitives :: [(String, (Int, PrimFunc a))]
primitives = [
  -- control / io primitives
  ("print",(-1,printPrimitive)),
  ("input",(0,inputPrimitive)),
  ("input-num",(0,inputNumPrimitive)),
  ("input-bool",(0,inputBoolPrimitive)),
  ("set",(2,setPrimitive)),
  ("fun",(2,funPrimitive)),
  ("if",(3,ifPrimitive)),
  ("while",(2,whilePrimitive)), -- let has a variable number of arguments
  ("for",(3,forPrimitive)),
  ("repeat",(2,repeatPrimitive)),
  ("let",(-1,letPrimitive)),
  ("random",(2,randomPrimitive)),
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
  ("!",(2,indexPrimitive)),
  -- string primitives
  ("concat",(-1,concatPrimitive))]

