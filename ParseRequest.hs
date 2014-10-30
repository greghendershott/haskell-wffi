module ParseRequest(
  parseRequestTemplate,
  RequestTemplate(..)
) where

import Text.ParserCombinators.Parsec
import KeyVal

data RequestTemplate = RequestTemplate
                       { rtMethod :: String
                       , rtPathParts :: [Value]
                       , rtQueryParams :: [KeyVal]
                       , rtHeaders :: [KeyVal]
                       , rtBody :: String
                       } deriving (Show)

parseRequestTemplate :: String -> Maybe RequestTemplate
parseRequestTemplate input =
  case (parse request "(unknown)" input) of
    (Right rt) -> Just rt
    (Left pe) -> Nothing

-- example usage:
-- parseTest request "GET /foo/bar.baz/users/{user}?k=0&k1=1"
-- parseTest request "GET /foo/bar.baz/users/{user}?q=0&r={}&s={alias}&[opt={}]&[copt=1]\nServer: Foo\nBar: Baz\n\n"

request :: GenParser Char st RequestTemplate
request = do
  many ws
  m <- method
  many1 $ char ' '
  pps <- many1 pathPart
  qps <- (option [] queryParams)
  hs <- (option [] headers)
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

-- Path parts

pathPart :: GenParser Char st Value
pathPart = try $ do
  (oneOf "/")
  (try variable) <|> pathConstant

pathConstant :: GenParser Char st Value
pathConstant = do
  cs <- many1 (noneOf "?/")
  return (Constant cs)

-- Query parameters

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
  (char '[')
  kv <- queryParamReq
  (char ']')
  return kv

queryKey :: GenParser Char st String
queryKey = many (alphaNum <|> oneOf "-_.")

queryVal = variable <|> queryValConstant

queryValConstant :: GenParser Char st Value
queryValConstant = do
  cs <- many (alphaNum <|> oneOf "-_.")
  return (Constant cs)

-- Headers

headers :: GenParser Char st [KeyVal]
headers = try $ do
  many ws
  xs <- many header
  -- (char '\n')
  return xs

header :: GenParser Char st KeyVal
header = try $ do
  h <- headerReq <|> headerOpt
  char '\n'
  return h

headerReq :: GenParser Char st KeyVal
headerReq = try $ do
  k <- headerKey
  many ws
  char ':'
  many ws
  v <- headerVal
  return (KeyVal k v)

headerOpt :: GenParser Char st KeyVal
headerOpt = try $ do
  (char '[')
  kv <- headerReq
  (char ']')
  return kv

headerKey :: GenParser Char st String
headerKey = many (alphaNum <|> oneOf "-_.")

headerVal = variable <|> headerValConstant

headerValConstant :: GenParser Char st Value
headerValConstant = do
  cs <- many (alphaNum <|> oneOf "-_.")
  return (Constant cs)

-- General

variable :: GenParser Char st Value
variable = try $ do
  cs <- between (char '{') (char '}') (many (noneOf "}"))
  return (case cs of
    "" -> (Variable Nothing)
    _  -> (Variable (Just cs)))

ws :: GenParser Char st [Char]
ws = (oneOf " \n") >> return []
