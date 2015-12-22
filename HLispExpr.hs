module HLispExpr (
  LispExec, LispEnv, PrimFunc,
  LispExpr(..)
) where

import Control.Monad.Except
import Control.Monad.State

import Data.List (intercalate)
import qualified Data.Map.Strict as M

-- LispState contains user-defined state and a global binding
-- the user-defined state is useful when defining special
-- primitive functions
type LispState a  = (a,LispEnv a)
type LispExec a   = ExceptT String (StateT (LispState a) IO) (LispExpr a)
type LispEnv  a   = M.Map String (LispExpr a)
type PrimFunc a   = LispEnv a -> [LispExpr a] -> LispExec a

data LispExpr a =
    LispList [LispExpr a]
  | LispQList [LispExpr a]
  | LispSym String
  | LispBool Bool
  | LispNum Int
  | LispStr String
  | LispUnit -- unit value
  | LispFunc [String] (LispExpr a) -- function: args, body
  | LispPrimFunc Int (PrimFunc a) -- primitive function: nargs, function object

instance Show (LispExpr a) where
  show (LispList l)         = "[" ++ (intercalate " " (map show l)) ++ "]"
  show (LispQList l)        = "~[" ++ (intercalate " " (map show l)) ++ "]"
  show (LispSym s)          = s
  show (LispBool b)         = show b
  show (LispNum n)          = show n
  show (LispStr s)          = "\"" ++ s ++ "\""
  show LispUnit             = "()"
  show (LispFunc args body) =
    "[\\" ++ (intercalate " " args) ++ " -> " ++ show body ++ "]"
  show (LispPrimFunc _ f)     = "(primitive function)"
