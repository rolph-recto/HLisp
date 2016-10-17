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
  | LispLFunc (LispEnv a) [String] (LispExpr a) -- lazy function (like fexprs)
  | LispPrimFunc Int (PrimFunc a) -- primitive function: nargs, function object

instance Show (LispExpr a) where
  show (LispList l)       = "[" ++ (intercalate " " (map show l)) ++ "]"
  show (LispQList l)        = "~[" ++ (intercalate " " (map show l)) ++ "]"
  show (LispSym s)        = s
  show (LispBool b)       = show b
  show (LispNum n)        = show n
  show (LispStr s)        = "\"" ++ s ++ "\""
  show LispUnit         = "()"
  show (LispFunc env args body) =
    let sargs = intercalate " " args in
    let senv  = intercalate ", " $ map (\(var,val)-> var ++ " -> " ++ show val) $ M.toList env in
    "[fun [" ++ sargs ++ "] " ++ show body ++ "] where [" ++ senv ++ "]"
  show (LispLFunc env args body) =
    let sargs = intercalate " " args in
    let senv  = intercalate ", " $ map (\(var,val)-> var ++ " -> " ++ show val) $ M.toList env in
    "[lfun [" ++ sargs ++ "] " ++ show body ++ "] where [" ++ senv ++ "]"
  show (LispPrimFunc _ f)     = "(primitive function)"

-- syntactic equality
instance Eq (LispExpr a) where
   (LispList l1) == (LispList l2) = all (uncurry (==)) $ zip l1 l2
   (LispQList l1) == (LispQList l2) = all (uncurry (==)) $ zip l1 l2
   (LispSym s1) == (LispSym s2) = s1 == s2
   (LispBool b1) == (LispBool b2) = b1 == b2
   (LispNum n1) == (LispNum n2) = n1 == n2
   (LispStr s1) == (LispStr s2) = s1 == s2
   (LispUnit) == (LispUnit) = True
   (LispFunc env1 args1 body1) == (LispFunc env2 args2 body2) =
     env1 == env2 && args1 == args2 && body1 == body2
   (LispLFunc env1 args1 body1) == (LispLFunc env2 args2 body2) =
     env1 == env2 && args1 == args2 && body1 == body2
   (LispPrimFunc  _ _) == (LispPrimFunc _ _) = False
   _ == _ = False

registerPrimitive :: LispEnv a -> String -> Int -> PrimFunc a -> LispEnv a
registerPrimitive env name n f = M.insert name (LispPrimFunc n f) env

registerPrimitives :: LispEnv a -> [(String, (Int, PrimFunc a))] -> LispEnv a
registerPrimitives env prims =
  foldr (\(name,(n,f)) acc -> M.insert name (LispPrimFunc n f) acc) env prims


