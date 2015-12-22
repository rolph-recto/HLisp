module HLispParse (parseLisp, parseLispFile) where

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
  exprs <- between (optional spaces >> char '[' >> optional spaces)
            (optional spaces >> char ']' >> optional spaces)
            (sepBy parseLispExpr spaces)
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
  let specialChars = "/!@#$%^&*-+_=<>|?"
  sym <- many1 $ choice [letter, digit, oneOf "/!@#$%^&*-+_=<>|?"]
  return $ LispSym sym

comment = char ';' >> commentBody
commentBody =
  (noneOf "\r\n" >> commentBody)
  <|> (newline >> return "")
  <|> (eof >> return "")

betweenExprs = do
  skipMany (many1 space <|> comment)

-- preprocess text input before parsing
preprocessInput :: String -> String
preprocessInput input = foldr (\f acc -> f acc) input processors
  where processors = [map toLower, strip]

parseLisp :: String -> Either ParseError LispExpr
parseLisp input = parse parseLispExpr "" (preprocessInput input)

parseLispFile :: String -> Either ParseError [LispExpr]
parseLispFile input = parse (optional betweenExprs >> sepEndBy parseLispExpr betweenExprs) "" (preprocessInput input)
