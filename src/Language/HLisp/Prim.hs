module Language.HLisp.Prim (hlispPrimitives) wherequit
:q


import System.IO
import System.Random
import System.Directory (doesFileExist)

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map.Strict as M
import Text.Read (readMaybe)
import Data.Char (toLower)
import Data.List (intercalate)

import Language.HLisp.Expr
import Language.HLisp.Eval
import Language.HLisp.Parse

evalPrimitive :: PrimFunc a
evalPrimitive env (expr':_) = do
  expr <- eval env expr'
  case expr of
    LispQList contents -> eval env (LispList contents)
    otherwise -> throwError "eval: expected quoted list as argument"

parsePrimitive :: PrimFunc a
parsePrimitive env (sexpr':_) = do
  sexpr <- eval env sexpr'
  case sexpr of
    LispStr str -> do
      case parseLisp str of
        Left err -> throwError (show err)
        Right expr -> do
          case expr of
            LispList contents -> return $ LispQList contents
            otherwise -> throwError "parse: string must be a list to be parsed"

    otherwise -> throwError "parse: expected string argument"

-- do nothing
nothingPrimitive :: PrimFunc a
nothingPrimitive _ _ = return LispUnit

-- call by current continuation
callccPrimitive :: PrimFunc a
callccPrimitive env (kfunc':_) = do
  kfunc <- eval env kfunc'
  case kfunc of
    LispFunc globalEnv localEnv [k] body -> do
      -- reify the current continuation into a lisp function
      contStack <- getLispStack
      let cont = foldr (.) id contStack
      let reifiedCont = LispFunc M.empty M.empty ["__CONT__"] $ cont (LispSym "__CONT__")
      liftIO $ print reifiedCont
      
      -- pass the cc into the argument of callcc
      -- smash the "control stack"
      applyFunc globalEnv localEnv "##CONTSYM##" [reifiedCont] [k] body
      
    otherwise -> do
      throwError "CallCC expects a function taking a single continuation object as an argument"

-- load a file
loadPrimitive :: PrimFunc a
loadPrimitive env (file':_) = do
  fval <- eval env file'
  case fval of
    LispStr file -> do
      fileExists <- liftIO $ doesFileExist file
      if fileExists
      then do
        h <- liftIO $ openFile file ReadMode
        filestr <- liftIO $ hGetContents h
        case parseLispFile filestr of
          Left err -> do
            liftIO $ hClose h
            throwError (show err)
        
          Right exprs -> do
            liftIO $ hClose h
            let exprList = LispList exprs
            eval env exprList

        else do
          throwError "load: file doesn't exist!"

    otherwise -> throwError "load: expected string argument"

-- print something out
printPrimitive :: PrimFunc a
printPrimitive env args = do
  vals <- mapM (eval env) args
  printstrs <- forM vals $ \val ->
    case val of
      LispStr s -> return s
      _         -> return (show val)

  liftIO $ putStrLn $ intercalate " " printstrs
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
-- syntax: [set a val]
-- add binding a=val to global environment
setPrimitive :: PrimFunc a
setPrimitive env args = do
  case args !! 0 of
    LispSym s -> do
      globalEnv <- getLispState
      val <- eval env (args !! 1)
      putLispState $ M.insert s val globalEnv
      return LispUnit

    otherwise -> throwError "set: first argument must be a symbol"

-- convert list to function
-- syntax [fun [arg1 arg2 ... argn] body]
-- creates a new function with args arg1 ... argn
funPrimitive :: (LispEnv a -> LispEnv a -> [String] -> LispExpr a -> LispExpr a) -> PrimFunc a
funPrimitive con env args = do
  case args of
    [LispList params, body@(LispList _)] -> do
      if all isSym params
      -- remember current env for closures
      then do
        globalEnv <- getLispState
        return $ con globalEnv env (map symStr params) body
      else throwError "fun: params must be symbols"

    _ -> throwError "fun: first and second arguments must be lists"

  where isSym (LispSym _)   = True
        isSym _             = False
        symStr (LispSym s)  = s

-- conditional expression
-- it's important that this has lazy semantics because branches could
-- have side effects so we shouldn't execute a branch until we know it's
-- the right one!
-- syntax: [if pred then else]
-- if pred is true, then is evaluated; otherwise else is evaluated
ifPrimitive :: PrimFunc a
ifPrimitive env args = do
  predVal <- eval env (args !! 0)
  case predVal of 
    LispBool True -> eval env (args !! 1)
    LispBool False -> eval env (args !! 2)
    otherwise -> throwError "if: predicate must be boolean"

-- syntax: [while pred body]
-- keep executing body over and over until pred is false
whilePrimitive :: PrimFunc a
whilePrimitive env (pred:body:_) = do
  predval <- eval env pred
  case predval of
    LispBool True   -> do
      eval env body
      whilePrimitive env [pred,body]
    LispBool False  -> return LispUnit
    otherwise       -> throwError "while: guard must be a boolean"

-- syntax: [for var lst body]
-- for every element in lst, add binding var=elem to env
-- in which body is evaluated
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

-- syntax: [case [x val] [[pred1 body1] [pred2 body2] ... [predn bodyn]]]
-- alternative: [case var [[pred1 body1] [pred2 body2] ... [predn bodyn]]]
-- used for case / switch statements. the predicates are evaluated one-by-one
-- with a binding x=val in the environment. the first predicate to evaluate
-- to true will have its concomitant body evaluated.
--
-- for the alternative syntax, var is assumed to be in the environment already
-- and the predicates are evaluated with the current environment
-- example:
-- [case [x [+ 2 2]] [
--   [[> x 100] [print "big number"]]
--   [[> x 50] [print "somewhat big number"]]
--   [[> x 10] [print "small number"]]
--   [true [print "small number"]]
--]]
-- returns the value of the body evaluated; otherwise if no body is evaluated
-- (all case preds are false) then it returns unit
evalCase :: PrimFunc a
evalCase env [] = return LispUnit
evalCase env (c:cs)
  | LispList [pred, body] <- c = do
    predval <- eval env pred
    case predval of
      LispBool b -> do
        if b then do
          bodyval <- eval env body
          return bodyval
        else evalCase env cs
  
      otherwise -> throwError "case predicate must be boolean"

  | otherwise = throwError "case has syntax [pred body]"

casePrimitive :: PrimFunc a
casePrimitive env (bind:casestruct:_) = do
  case (bind,casestruct) of
    (LispList [LispSym var,bindval], LispList cases) -> do
      bindval' <- eval env bindval
      let env' = M.insert var bindval' env
      evalCase env' cases

    (LispSym var, LispList cases) -> do
      if var `M.member` env then do
        evalCase env cases
      else do
        lstate <- getLispState
        if var `M.member` lstate then do
          evalCase lstate cases
        else throwError $ "var " ++ show var ++ " is not in the environment"

    _ -> throwError "case: syntax is [case [x val] [[pred1 body1] ... [predn bodyn]]]"

-- syntax: [repeat n body]
-- repeat evaluation of body n times
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
-- syntax: [let [x valx] [y valy] ... body]
-- add bindings x=valx, y=valy ... to evaluation of body
letPrimitive :: PrimFunc a
letPrimitive env args = do
  let (body:letbinds) = reverse args 
  binds <- forM (reverse letbinds) $ \letbind -> do
    case letbind of
      LispList [LispSym var, val] -> return (var,val)
      otherwise -> do
        throwError "let: binds must have form [let [var val] ... [var val] body]"

  let (vars, vals) = unzip binds
  globalEnv <- getLispState
  applyFunc globalEnv env "let" vals vars body

-- random number generator
-- syntax: [random lo hi]
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
    _ -> throwError $ fsym ++ " takes two integers"

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
  liftIO $ print lst
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
indexPrimitive env (lst:index:_) = do
  lstVal <- eval env lst
  indexVal <- eval env index
  case (lstVal, indexVal) of
    (LispQList qlst, LispNum n) -> do
      if n < length qlst
      then return $ qlst !! n
      else throwError "list index out of bounds"
    (LispStr str, LispNum n) -> do
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
hlispPrimitives :: [(String, (Int, PrimFunc a))]
hlispPrimitives = [
  -- 'reflection' primitives
  ("eval",(1,evalPrimitive)),
  ("parse",(1,parsePrimitive)),
  ("nothing",(0,nothingPrimitive)),
  ("callcc",(1,callccPrimitive)),
  -- io primitives
  ("load",(1,loadPrimitive)),
  ("print",(-1,printPrimitive)),
  ("input",(0,inputPrimitive)),
  ("input-num",(0,inputNumPrimitive)),
  ("input-bool",(0,inputBoolPrimitive)),
  -- control primitives
  ("set",(2,setPrimitive)),
  ("fun",(2,funPrimitive LispFunc)),
  ("lfun",(2,funPrimitive LispLFunc)),
  ("if",(3,ifPrimitive)),
  ("while",(2,whilePrimitive)), -- let has a variable number of arguments
  ("for",(3,forPrimitive)),
  ("case",(2,casePrimitive)),
  ("repeat",(2,repeatPrimitive)),
  ("let",(-1,letPrimitive)),
  -- arith + bool primitives
  -- we give these mangled names so we can allow
  -- partial application with the unmangled names,
  -- which are defined as lisp functions
  ("__PRIM__random",(2,randomPrimitive)),
  ("__PRIM__+",(2,addPrimitive)),
  ("__PRIM__-",(2,subPrimitive)),
  ("__PRIM__*",(2,mulPrimitive)),
  ("__PRIM__/",(2,divPrimitive)),
  ("__PRIM__<",(2,ltPrimitive)),
  ("__PRIM__>",(2,gtPrimitive)),
  ("__PRIM__==",(2,eqPrimitive)),
  -- list primitives
  ("__PRIM__head",(1,headPrimitive)),
  ("__PRIM__tail",(1,tailPrimitive)),
  ("__PRIM__nil?",(1,nilPrimitive)),
  ("__PRIM__cons",(2,consPrimitive)),
  ("__PRIM__!",(2,indexPrimitive)),
  -- string primitives
  ("concat",(-1,concatPrimitive))]

