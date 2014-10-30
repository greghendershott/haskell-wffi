module Wffi.ParseURL(
  -- parseRequestTemplate
) where

import Text.ParserCombinators.Parsec
import Wffi.KeyVal

data RequestTemplate = RequestTemplate
                       { rtMethod :: String
                       , rtPathParts :: [Value]
                       , rtQueryParams :: [KeyVal]
                       , rtHeaders :: [KeyVal]
                       , rtBody :: String
                       } deriving (Show)

parseRequestTemplate :: String -> Either ParseError RequestTemplate
parseRequestTemplate input = parse request "(unknown)" input

request :: GenParser Char st RequestTemplate
request = do
  many ws
  m <- method
  many1 $ char ' '
  pps <- many1 pathPart
  qps <- queryParams
  hs <- return [] -- (optional headers [])
  body <- return "" -- FIXME
  many ws
  eof
  return (RequestTemplate m pps qps hs body)
  
method :: GenParser Char st String
method = do
  (try $ string "GET")
    <|> (try $ string "PUT")
    <|> (try $ string "POST")
    <|> (try $ string "DELETE")
    <|> (try $ string "PATCH")

pathPart :: GenParser Char st Value
pathPart = try $ do
  dummy <- (oneOf "/")
  (try variable) <|> pathConstant

pathConstant :: GenParser Char st Value
pathConstant = do
  cs <- many1 (noneOf "?/")
  return (Constant cs)

queryParams :: GenParser Char st [KeyVal]
queryParams = try $ do
  many ws
  char '?'
  xs <- sepBy (queryParamOpt <|> queryParamReq) (char '&')
  return xs

queryParamReq :: GenParser Char st KeyVal
queryParamReq = try $ do
  k <- queryKey
  char '='
  v <- queryVal
  return (KeyVal k v)

queryParamOpt :: GenParser Char st KeyVal
queryParamOpt = try $ do
  char '['
  k <- queryKey
  char '='
  v <- queryVal
  char ']'
  return (KeyVal k v)

queryKey :: GenParser Char st String
queryKey = many (alphaNum <|> oneOf "-_.")

queryVal = variable <|> queryValConstant

queryValConstant :: GenParser Char st Value
queryValConstant = do
  cs <- many (alphaNum <|> oneOf "-_.")
  return (Constant cs)

variable :: GenParser Char st Value
variable = try $ do
  cs <- between (char '{') (char '}') (many1 (noneOf "}"))
  return (Variable (Just cs) "")

ws :: GenParser Char st [Char]
ws = (oneOf " \n") >> return []

---------------
