module Language.HLisp.Expr (
  LispExec, LispEnv, PrimFunc,
  LispExpr(..),
  LispState,
  registerPrimitive,
  registerPrimitives,
  getUserState, getLispState, putUserState, putLispState
) where

import Control.Monad.Except
import Control.Monad.State

import Data.List (intercalate)
import qualified Data.Map.Strict as M

-- LispState contains user-defined state and a global binding
-- the user-defined state is useful when defining special
-- primitive functions
type LispEnv  a     = M.Map String (LispExpr a)
type LispState a    = (a,LispEnv a)
-- action on the monad stack
-- type LispAction a b = EitherT String ReaderT (LispEnv a) (StateT (LispState a) IO) b
type LispAction a b = ExceptT String (StateT (LispState a) IO) b
-- an execution action (primitive function, etc.)
type LispExec a     = LispAction a (LispExpr a)
type PrimFunc a     = LispEnv a -> [LispExpr a] -> LispExec a

getUserState :: LispAction a a
getUserState = fst <$> get 

getLispState :: LispAction a (LispEnv a)
getLispState = snd <$> get 

putUserState :: a -> LispAction a ()
putUserState ustate = do
  (_,lstate) <- get
  put (ustate,lstate)

putLispState :: LispEnv a -> LispAction a ()
putLispState lstate = do
  (ustate,_) <- get
  put (ustate,lstate)

data LispExpr a =
    LispList [LispExpr a]
  | LispQList [LispExpr a]
  | LispSym String
  | LispBool Bool
  | LispNum Int
  | LispStr String
  | LispUnit -- unit value
  | LispFunc (LispEnv a) [String] (LispExpr a) -- function: closure env, args, body
  | LispPrimFunc Int (PrimFunc a) -- primitive function: nargs, function object

instance Show (LispExpr a) where
  show (LispList l)       = "[" ++ (intercalate " " (map show l)) ++ "]"
  show (LispQList l)        = "~[" ++ (intercalate " " (map show l)) ++ "]"
  show (LispSym s)        = s
  show (LispBool b)       = show b
  show (LispNum n)        = show n
  show (LispStr s)        = "\"" ++ s ++ "\""
  show LispUnit         = "()"
  show (LispFunc _ args body) =
    "[fun [" ++ (intercalate " " args) ++ "] " ++ show body ++ "]"
  show (LispPrimFunc _ f)     = "(primitive function)"

registerPrimitive :: LispEnv a -> String -> Int -> PrimFunc a -> LispEnv a
registerPrimitive env name n f = M.insert name (LispPrimFunc n f) env

registerPrimitives :: LispEnv a -> [(String, (Int, PrimFunc a))] -> LispEnv a
registerPrimitives env prims =
  foldr (\(name,(n,f)) acc -> M.insert name (LispPrimFunc n f) acc) env prims


