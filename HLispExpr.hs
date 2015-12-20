module HLispExpr (
  LispExec, LispEnv, PrimFunc,
  LispExpr(..)
) where

import Control.Monad.Except
import Control.Monad.State

import Data.List (intercalate)
import qualified Data.Map.Strict as M

type LispExec     = ExceptT String (StateT LispEnv IO) LispExpr
type LispEnv      = M.Map String LispExpr
type PrimFunc     = LispEnv -> [LispExpr] -> LispExec

data LispExpr =
    LispList [LispExpr]
  | LispQList [LispExpr]
  | LispSym String
  | LispBool Bool
  | LispNum Int
  | LispStr String
  | LispUnit -- unit value
  | LispFunc [String] LispExpr -- function: args, body
  | LispPrimFunc Int PrimFunc -- primitive function: nargs, function object

instance Show LispExpr where
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
