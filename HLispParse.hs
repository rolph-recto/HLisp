module HLispParse (parseLisp) where

import Data.Char
import Data.String.Utils
import Text.ParserCombinators.Parsec

import HLispExpr

-- parsing
parseLispExpr = parseLispQList
            <|> try parseLispList
            <|> try parseLispBool
            <|> try parseLispNum
            <|> try parseLispStr
            <|> try parseLispSym

parseLispQList = do
  exprs <- between (string "~[") (char ']') (sepBy parseLispExpr spaces)
  return $ LispQList exprs

parseLispList = do
  exprs <- between (char '[') (char ']') (sepBy parseLispExpr spaces)
  return $ LispList exprs

parseLispBoolTrue = do
  string "true"
  return $ LispBool True

parseLispBoolFalse = do
  string "false"
  return $ LispBool False

parseLispBool = parseLispBoolTrue <|> try parseLispBoolFalse

parseLispNum = do
  numstr <- many1 digit
  let num = read numstr :: Int
  return $ LispNum num

parseLispStr = do
  str <- between (char '"') (char '"') (many1 $ noneOf "\"")
  return $ LispStr str

parseLispSym = do
  sym <- many1 $ choice [letter, oneOf "/!@#$%^&*-+_=<>|?"]
  return $ LispSym sym

-- preprocess text input before parsing
preprocessInput :: String -> String
preprocessInput input = foldr (\f acc -> f acc) input processors
  where processors = [map toLower, strip]

parseLisp :: String -> Either ParseError LispExpr
parseLisp input = parse parseLispExpr "" (preprocessInput input)

