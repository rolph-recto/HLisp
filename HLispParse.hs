module HLispParse (parseLisp, parseLispFile) where

import Data.Char
import Text.ParserCombinators.Parsec

import HLispExpr

-- taken from Data.String.Utils, so that
-- we don't enter Cabal Hell by importing MissingH
wschars :: String
wschars = " \t\r\n"

strip :: String -> String
strip = lstrip . rstrip

-- | Same as 'strip', but applies only to the left side of the string.
lstrip :: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if elem x wschars
                            then lstrip xs
                            else s

-- | Same as 'strip', but applies only to the right side of the string.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

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
  neg <- optionMaybe (char '-')
  numstr <- many1 digit
  let num = read numstr :: Int
  case neg of 
    Just _ -> return $ LispNum (-num)
    Nothing -> return $ LispNum num

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

parseLisp :: String -> Either ParseError (LispExpr a)
parseLisp input = parse parseLispExpr "" (preprocessInput input)

parseLispFile :: String -> Either ParseError [LispExpr a]
parseLispFile input = parse (optional betweenExprs >> sepEndBy parseLispExpr betweenExprs) "" (preprocessInput input)
