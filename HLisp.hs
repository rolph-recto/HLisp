module Main (main) where

import System.IO
import System.Directory (doesFileExist)

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map.Strict as M
import Data.List (intercalate)

import HLispExpr
import HLispParse
import HLispEval
import HLispPrim

main = do
  hSetBuffering stdout NoBuffering
  putStrLn "HLisp v0.01"
  -- add primitives to global env here
  let globalEnv = foldr insertPrim M.empty primitives
  mainLoop ((),globalEnv)
  where insertPrim (bind,(n,f)) acc = M.insert bind (LispPrimFunc n f) acc


mainLoop state@(userState, lispState) = do
  putStr ">> "
  line <- getLine
  case words line of
    -- repl commands
    [] -> mainLoop state

    ("quit":_) -> return ()

    ("globals":_) -> do
      let userGlobals = filter isUserGlobal $ M.toList lispState
      if length userGlobals > 0
      then do
        let userBinds = map fst userGlobals
        liftIO $ putStr "User-defined globals: "
        liftIO $ putStrLn $ intercalate " " userBinds
        mainLoop state

      else do
        liftIO $ putStrLn "No user-defined globals."
        mainLoop state

    ("clearglobals":_) -> do
      let lispState' = M.fromList $ filter (not . isUserGlobal) $ M.toList lispState
      liftIO $ putStrLn "Removed all user-defined globals."
      mainLoop (userState, lispState')

    ("info":bind:_) -> do
      case M.lookup bind lispState of
        Just val -> do
          liftIO $ putStrLn $ bind ++ " : " ++ (show val)
          mainLoop state
        Nothing -> do
          liftIO $ putStrLn $ "No binding found for " ++ bind ++ "."
          mainLoop state
    
    ("load":file:_) -> do
      fileExists <- doesFileExist file
      if fileExists
      then do
        withFile file ReadMode $ \h -> do
          filestr <- hGetContents h
          case parseLispFile filestr of
            Left err -> do
              liftIO $ putStrLn $ show err
              mainLoop state
          
            Right exprs -> do
              let exprList = LispList exprs
              (result, state') <- runStateT (runExceptT $ eval M.empty exprList) state
              case result of
                Left err -> do
                  liftIO $ putStrLn err
                  mainLoop state'

                Right _ -> do
                  mainLoop state'

        else do
          liftIO $ putStrLn "File doesn't exist!"
          mainLoop state


    -- a lisp command
    otherwise -> do
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

    where isPrim (LispPrimFunc _ _) = True
          isPrim _                  = False
          isUserGlobal (key,val)    = not $ isPrim val
        
