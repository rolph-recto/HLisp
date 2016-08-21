{-# LANGUAGE JavaScriptFFI #-}

module Main (main) where

import System.IO.Unsafe
import Data.IORef
import Control.Applicative ((<$>))
import Control.Monad (forM)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.List.Split as Split

import GHCJS.Marshal(fromJSVal, toJSVal)
import GHCJS.Foreign.Callback (Callback, syncCallback1')
import Data.JSString (JSString, unpack, pack)
import GHCJS.Types (JSVal)

import Language.HLisp.Expr
import Language.HLisp.Parse
import Language.HLisp.Eval
import Language.HLisp.Prelude

-- a really hacky way of getting a global variable
hlispEnv :: IORef (LispEnv a)
hlispEnv = unsafePerformIO $ newIORef hlispPrelude

hlispRepl :: String -> IO String
hlispRepl input = do
  lispState <- readIORef hlispEnv

  case words input of
    -- repl commands
    [] -> return ""

    ("globals":_) -> do
      let userGlobals = filter isUserGlobal $ M.toList lispState
      if length userGlobals > 0
      then do
        let userBinds = map fst userGlobals
        return $ L.intercalate " " userBinds

      else do
        return "No user-defined globals."

    ("clearglobals":_) -> do
      let lispState' = M.fromList $ filter (not . isUserGlobal) $ M.toList lispState
      writeIORef hlispEnv lispState'
      return "Removed all user-defined globals."

    ("info":bind:_) -> do
      case M.lookup bind lispState of
        Just val -> do
          return $ bind ++ " : " ++ (show val)
        Nothing -> do
          return $ "No binding found for '" ++ bind ++ "'."

    ("help":_) -> do
      let cmds = [("globals", "show all user-defined globals")
                 , ("clearglobals", "remove all globals")
                 , ("info [binding]", "get information about a binding")]
            
      let output = ["Available commands:"]
      let output' = output ++ map (\(cmd,desc) -> "- " ++ cmd ++ ": " ++ desc) cmds
      return $ L.intercalate "\n" output'

    -- a lisp command
    otherwise -> do
      case parseLisp input of
        Left err -> return $ show err
        Right expr -> do
          (res, (_, lispState')) <- runLisp ((),lispState) expr
          writeIORef hlispEnv lispState'
          case res of
            Left err -> return $ show err
            Right val -> do
              if val == LispUnit
              then return ""
              else return $ show val

    where isPrim (LispPrimFunc _ _) = True
          isPrim _                  = False
          isUserGlobal (key,val)    = not $ isPrim val

hlispRepl' :: JSVal -> IO JSVal
hlispRepl' jsval = do
  Just str <- fromJSVal jsval
  let input = T.unpack $ T.strip $ T.pack $ unpack str
  if length input > 0
  then do
    res <- hlispRepl input
    -- wrap output to make it more readable
    let chunks = map (L.intercalate "\n" . Split.chunksOf 120) $ lines res
    let res'   = L.intercalate "\n" chunks
    toJSVal $ pack res'
  else toJSVal $ pack ""

-- for the FFI, I followed this:
-- http://stackoverflow.com/questions/29967135/how-to-call-haskell-from-javascript-with-ghcjs
foreign import javascript unsafe "hlisp = $1"
  set_callback :: Callback a -> IO ()

main = do
    replCallback <- syncCallback1' hlispRepl'
    set_callback replCallback
