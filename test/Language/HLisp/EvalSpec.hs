module Language.HLisp.EvalSpec (main, spec) where

import Test.Hspec

import qualified Data.Map.Strict as M

import Language.HLisp.Expr
import Language.HLisp.Eval
import Language.HLisp.Prim
import Language.HLisp.Prelude
import Language.HLisp.Parse

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "eval" $ do
    it "adds properly" $ do
      let expr = parse "[+ 1 1]"
      expr `shouldEvalTo` LispNum 2

    it "maps properly" $ do
      let expr = parse "[map [+ 2] ~[1 2 3]]"
      expr `shouldEvalTo` LispQList [LispNum 3, LispNum 4, LispNum 5]

    it "lengths properly" $ do
      let expr = parse "[length [map [+ 2] ~[1 2 3]]]"
      expr `shouldEvalTo` LispNum 3

    it "does evaluation properly" $ do
      let expr = parse "[eval ~[length [map [+ 2] ~[1 2 3]]]]"
      expr `shouldEvalTo` LispNum 3

  where env = registerPrimitives hlispPrelude hlispPrimitives
        eval expr = fst <$> runLisp ((), env) expr
        expr `shouldEvalTo` expected = do
          res <- eval expr
          case res of
            Left msg -> expectationFailure msg
            Right val -> val `shouldBe` expected
        parse sexpr = case parseLisp sexpr of
          Left msg -> error (show msg)
          Right expr -> expr

