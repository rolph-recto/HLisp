module HLisp () where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as M

import Data.Char
import Data.String.Utils
import Data.List (intercalate)

import Text.ParserCombinators.Parsec

data LispExpr =
    LispList [LispExpr]
  | LispQList [LispExpr]
  | LispSym String
  | LispBool Bool
  | LispNum Int
  | LispStr String
  | LispUnit -- unit value
  | LispFunc [String] LispExpr -- function: args, body

instance Show LispExpr where
  show (LispList l)         = "<" ++ (intercalate " " (map show l)) ++ ">"
  show (LispQList l)         = "<" ++ (intercalate " " (map show l)) ++ ">"
  show (LispSym s)          = s
  show (LispBool b)         = show b
  show (LispNum n)          = show n
  show (LispStr s)          = "\"" ++ s ++ "\""
  show LispUnit             = "()"
  show (LispFunc args body) =
    "<\\" ++ (intercalate " " args) ++ " -> " ++ show body ++ ">"

-- parsing
parseLispExpr = parseLispQList
            <|> try parseLispList
            <|> try parseLispBool
            <|> try parseLispNum
            <|> try parseLispStr
            <|> try parseLispSym

parseLispQList = do
  exprs <- between (string "`<") (char '>') (sepBy1 parseLispExpr spaces)
  return $ LispQList exprs

parseLispList = do
  exprs <- between (char '<') (char '>') (sepBy1 parseLispExpr spaces)
  return $ LispList exprs

parseLispBoolTrue = do
  string "true"
  return $ LispBool True

parseLispBoolFalse = do
  string "false"
  return $ LispBool False

parseLispBool = parseLispBoolTrue <|> try parseLispBoolFalse

parseLispNum = do
  numstr <- many1 digit
  let num = read numstr :: Int
  return $ LispNum num

parseLispStr = do
  str <- between (char '"') (char '"') (many1 $ noneOf "\"")
  return $ LispStr str

parseLispSym = do
  sym <- many1 $ choice [letter, oneOf "/!@#$%^&*-+_"]
  return $ LispSym sym

-- preprocess text input before parsing
preprocessInput :: String -> String
preprocessInput input = foldr (\f acc -> f acc) input processors
  where processors = [map toLower, strip]

parseLisp :: String -> Either ParseError LispExpr
parseLisp input = parse parseLispExpr "" (preprocessInput input)

type LispExec = ExceptT String (StateT LispEnv IO) LispExpr
type LispEnv  = M.Map String LispExpr

-- primitive functions and number of parameters they take
primitives = [
  ("print",(1,printPrimitive)),
  ("set",(2,setPrimitive)),
  ("fun",(2,funPrimitive)),
  ("if",(3,ifPrimitive)),
  ("+",(2,addPrimitive)),
  ("-",(2,subPrimitive)),
  ("*",(2,mulPrimitive)),
  ("/",(2,divPrimitive))]
  
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
  | Just (numParams, primFunc) <- fsym `lookup` primitives = do
    let numArgs = length args
    if numParams == numArgs
    then primFunc env args
    else throwError $ fsym ++ " expression expects " ++ show numParams ++ " arguments, got " ++ show numArgs
  | otherwise = do
    case M.lookup fsym env of
      Just (LispFunc params body) -> applyFunc env fsym args params body
      otherwise -> do
        globalEnv <- lift get
        case M.lookup fsym globalEnv of
          Just (LispFunc params body) -> applyFunc env fsym args params body
          otherwise -> throwError $ fsym ++ " is not a function"

-- primitive functions
printPrimitive :: LispEnv -> [LispExpr] -> LispExec
printPrimitive env (arg:_) = do
  val <- eval env arg
  case val of
    LispStr s -> liftIO $ putStrLn s
    _         -> liftIO $ putStrLn $ show val

  return LispUnit
  
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


main = do
  putStrLn "HLisp v0.01"
  mainLoop M.empty

mainLoop state = do
  putStr ">> "
  line <- getLine
  unless (line == "quit") $ do
    case parseLisp line of
      Left err -> do
        liftIO $ putStrLn $ show err
        mainLoop state
    
      Right expr -> do
        (result, state') <- runStateT (runExceptT $ eval M.empty expr) state
        case result of
          Left err -> do
            liftIO $ putStrLn err
            mainLoop state'

          -- don't print anything for unit values
          Right LispUnit -> do
            mainLoop state'
          
          Right val -> do
            liftIO $ putStrLn $ show val
            mainLoop state'
