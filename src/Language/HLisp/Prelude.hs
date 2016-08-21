module Language.HLisp.Prelude (hlispPrelude) where

import Data.Either
import qualified Data.Map.Strict as M

import Language.HLisp.Expr
import Language.HLisp.Parse
import Language.HLisp.Eval
import Language.HLisp.Prim

hlispPrelude :: LispEnv a
hlispPrelude =
  let pstr = [("random", "[fun [lo hi] [__PRIM__random lo hi]]")
              , ("+", "[fun [x y] [__PRIM__+ x y]]")
              , ("-", "[fun [x y] [__PRIM__- x y]]")
              , ("*", "[fun [x y] [__PRIM__* x y]]")
              , ("/", "[fun [x y] [__PRIM__/ x y]]")
              , ("<", "[fun [x y] [__PRIM__< x y]]")
              , (">", "[fun [x y] [__PRIM__> x y]]")
              , ("==", "[fun [x y] [__PRIM__== x y]]")
              , ("head", "[fun [x] [__PRIM__head x]]")
              , ("tail", "[fun [x] [__PRIM__tail x]]")
              , ("nil?", "[fun [x] [__PRIM__nil? x]]")
              , ("cons", "[fun [h t] [__PRIM__cons h t]]")
              , ("!", "[fun [l n] [__PRIM__! l n]]")
              , ("and", "[fun [x y] [if x [if y true false] false]]")
              , ("or", "[fun [x y]  [if x true [if y true false]]]")
              , ("not", "[fun [x] [if x false true]]")
              , ("any", "[fun [f lst] [foldl [fun [x acc] [or [f x] acc]] false lst]]")
              , ("all", "[fun [f lst] [foldl [fun [x acc] [and [f x] acc]] true lst]]")
              , (">=", "[fun [x y] [or [> x y] [== x y]]]")
              , ("<", "[fun [x y] [or [< x y] [== x y]]]")
              , ("map", "[fun [f lst] [if [nil? lst] ~[] [cons [f [head lst]] [map f [tail lst]]]]]")
              , ("filter", "[fun [f lst] [if [nil? lst] ~[] [let [hd [head lst]] [tl [tail lst]] [if [f hd] [cons hd [filter f tl]] [filter f tl]]]]]")
              , ("foldr", "[fun [f acc lst] [if [nil? lst] acc [f [head lst] [foldr f acc [tail lst]]]]]")
              , ("foldl", "[fun [f acc lst] [if [nil? lst] acc [foldl f [f acc [head lst]] [tail lst]]]]")
              , ("length", "[fun [lst] [foldl [fun [acc x] [+ 1 acc]] 0 lst]]")
              , ("range", "[fun [lo hi] [if [== lo hi] ~[] [cons lo [range [+ lo 1] hi]]]]")
              , ("append", "[fun [x y] [if [nil? x] y [cons [head x] [append [tail x] y]]]]")
              , ("cycle", "[fun [n i lst] [[if [< n 1] ~[] [if [< i [length lst]] [cons [! lst i] [cycle [- n 1] [+ i 1] lst]] [cons [! lst 0] [cycle [- n 1] 1 lst]]]]]]")] in
  let msg = "parse error in prelude!" in
  let processMapping = fmap (either (const $ error msg) id . parseLisp) in
  let pexprs = map processMapping pstr in
  let env = foldr (uncurry M.insert) M.empty pexprs in
  registerPrimitives env hlispPrimitives
