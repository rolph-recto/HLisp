import System.IO
import System.Directory (doesFileExist)
import System.Console.ANSI

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map.Strict as M
import Data.List (intercalate)

import Language.HLisp.Expr
import Language.HLisp.Parse
import Language.HLisp.Eval
import Language.HLisp.Prim
import HLispPrelude

consoleColor color act = do
  setSGR [SetColor Foreground Vivid color]
  act
  setSGR [Reset]

consoleWarn = consoleColor Red 
consoleOkay = consoleColor Green
consoleInfo = consoleColor White

main = do
  hSetBuffering stdout NoBuffering
  setSGR [SetColor Foreground Vivid White, SetUnderlining SingleUnderline]
  putStrLn "HLisp v0.01"
  setSGR [Reset]
  -- add primitives to global env here
  let globalEnv = registerPrimitives M.empty primitives
  -- load prelude
  case parseLispFile hlispPrelude of
    Left err -> do
      putStrLn "Error loading prelude! Exiting..."

    Right exprs -> do
      let exprList = LispList exprs
      (_, globalEnv') <- runLisp ((),globalEnv) exprList
      mainLoop globalEnv'

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
        liftIO $ consoleOkay $ putStr "User-defined globals: "
        liftIO $ putStrLn $ intercalate " " userBinds
        mainLoop state

      else do
        liftIO $ consoleWarn $ putStrLn "No user-defined globals."
        mainLoop state

    ("clearglobals":_) -> do
      let lispState' = M.fromList $ filter (not . isUserGlobal) $ M.toList lispState
      liftIO $ consoleOkay $ putStrLn "Removed all user-defined globals."
      mainLoop (userState, lispState')

    ("info":bind:_) -> do
      case M.lookup bind lispState of
        Just val -> do
          liftIO $ consoleInfo $ putStr bind
          liftIO $ putStrLn $ " : " ++ (show val)
          mainLoop state
        Nothing -> do
          liftIO $ consoleWarn $ putStr $ "No binding found for "
          liftIO $ consoleInfo $ putStrLn $ bind ++ "."
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
              (result, state') <- runLisp state exprList
              case result of
                Left err -> do
                  liftIO $ consoleWarn $ putStrLn err
                  mainLoop state'

                Right _ -> do
                  liftIO $ consoleOkay $ putStrLn "File loaded."
                  mainLoop state'

        else do
          liftIO $ consoleWarn $ putStrLn "File doesn't exist!"
          mainLoop state

    ("help":_) -> do
      liftIO $ consoleInfo $ putStrLn "Available commands:"
      let cmds = [("globals", "show all user-defined globals")
               ,  ("load [file]", "load and execute an HLisp file")
               ,  ("clearglobals", "remove all globals")
               ,  ("quit", "exit repl")]
      forM cmds printCmd
      mainLoop state
      where printCmd (cmd,def) = liftIO $ do
              putStr "- "
              consoleInfo $ putStr $ cmd ++ ": "
              putStrLn def

    -- a lisp command
    otherwise -> do
      case parseLisp line of
        Left err -> do
          liftIO $ putStrLn $ show err
          mainLoop state
      
        Right expr -> do
          (result, state') <- runLisp state expr
          case result of
            Left err -> do
              liftIO $ consoleWarn $ putStrLn err
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
        
