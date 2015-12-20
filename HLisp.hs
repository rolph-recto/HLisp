module HLisp () where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map.Strict as M

import HLispExpr
import HLispParse
import HLispEval
import HLispPrim

main = do
  putStrLn "HLisp v0.01"
  -- add primitives to global env here
  let globalEnv = foldr insertPrim M.empty primitives
  mainLoop globalEnv
  where insertPrim (bind,(n,f)) acc = M.insert bind (LispPrimFunc n f) acc

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
