{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Wffi(
  markdownToService,
  ApiFunction(..),
  Service(..),
  doRequest
) where

import Data.Maybe (mapMaybe)
import Data.Set (insert, delete)
import Data.List (intersperse)
import Data.Maybe (fromJust)
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Readers.Markdown
import Text.Show.Functions -- to Show function member of ApiFunction
import qualified Network.HTTP as H
import qualified Network.HTTP.Headers as HH
import qualified Network.URI as U
import ParseRequest
import KeyVal
import PlainText

type Args = [(String, String)] -- Named args, i.e. a dictionary

data ApiFunction = ApiFunction { apiName :: String
                               , apiDescription :: String
                               , requestTemplateRaw :: Maybe String -- debug
                               , requestTemplate :: Maybe RequestTemplate
                               , request :: Args -> IO (Either String String)
                               } deriving (Show)

data Service = Service { serviceName :: String
                       , serviceDescription :: String
                       , endpoint :: String
                       , functions :: [ApiFunction]
                       } deriving (Show)

markdownToService :: String -> Service
markdownToService s =
  let pd = markdownToPandoc s
      sections = h1Sections pd
      intro = head sections
      name = inlinesToPlainText $ case intro of ((Header 1 _ x) : _) -> x
      desc = blocksToPlainText $ case intro of ((Header 1 _ _) : x) -> x
      ep = findEndpoint intro
  in Service { serviceName = name,
               serviceDescription = desc,
               endpoint = ep,
               functions = map (sectionToApiFunction ep) $ tail sections }

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def
  {
    readerSmart = True,
    readerExtensions = delete Ext_implicit_figures $
                       insert Ext_backtick_code_blocks $
                       insert Ext_fenced_code_blocks $
                       readerExtensions def
  }

markdownToPandoc :: String -> Pandoc
markdownToPandoc s = Text.Pandoc.Readers.Markdown.readMarkdown def s

h1Sections :: Pandoc -> [[Block]]
h1Sections (Pandoc _ blocks) = gatherBy h1Pred blocks

h1Pred :: Block -> Bool
h1Pred (Header 1 _ _) = True
h1Pred  _             = False

findEndpoint :: [Block] -> String
findEndpoint [] = "not-found"
findEndpoint ((Para [Str "Endpoint:", Space, Str url]) : _) = url
findEndpoint (_ : xs) = findEndpoint xs

sectionToApiFunction :: String -> [Block] -> ApiFunction
sectionToApiFunction endpoint blocks =
  let name = inlinesToPlainText $ case blocks of ((Header 1 _ x) : _) -> x
      desc = blocksToPlainText $ case blocks of ((Header 1 _ _) : x) -> x
      rawrt = findRequestTemplate blocks
      rt = case rawrt of
        (Just x) -> parseRequestTemplate x
        Nothing  -> Nothing
      f = case rt of
        (Just t) -> requestTemplateToWrapper t endpoint
        Nothing  -> (\_ -> do return $ Left "could not parse request template")
  in ApiFunction{ apiName = name,
                  apiDescription = desc,
                  requestTemplateRaw = rawrt,
                  requestTemplate = rt,
                  request = f }

findRequestTemplate :: [Block] -> Maybe String
findRequestTemplate []                    = Nothing
findRequestTemplate ((CodeBlock _ s) : _) = Just s
findRequestTemplate (_:xs)                = findRequestTemplate xs

requestTemplateToWrapper :: RequestTemplate -> String -> (Args -> IO (Either String String))
requestTemplateToWrapper rt endpoint = doRequest rt endpoint

doRequest :: RequestTemplate -> String -> Args -> IO (Either String String)
doRequest rt endpoint args =
  let method = rtMethod rt
      missing what name =
        error $ "Missing required " ++ what ++ ": `" ++ name ++ "'"
      -- Path segments
      pathValue x =
        case x of
          (Constant s)        -> s
          (Variable (Just k)) -> (case lookup k args of
                                     (Just s) -> s
                                     _        -> missing "path part" k)
          _                   -> error "path error"
      paths = (map pathValue (rtPathParts rt))
      path = endpoint ++ "/" ++ (foldl1 (++) (intersperse "/" paths))
      -- Some code that can be shared among query params and headers
      paramValue sk fk x =
        case x of
          (KeyVal k (Constant v))        -> sk k v
          (KeyVal _ (Variable (Just k))) -> (case lookup k args of
                                                (Just v) -> sk k v
                                                _        -> fk k)
          (KeyVal k (Variable Nothing))  -> (case lookup  k args of
                                                (Just v) -> sk k v
                                                _        -> fk k)
          (KeyVal k (Optional v)) -> paramValue sk (\_ -> Nothing) (KeyVal k v)
      -- Query parameters
      justQuery k v = Just $ k ++ "=" ++ v
      queries = mapMaybe (paramValue justQuery (missing "query parameter")) (rtQueryParams rt)
      query = case queries of
        [] -> ""
        xs -> "?" ++ (foldl1 (++) (intersperse "&" xs))
      -- Headers
      justHead k v = Just $ HH.Header (HH.HdrCustom k) v
      heads = mapMaybe (paramValue justHead (missing "header")) (rtHeaders rt)
      -- Assemble the Request
      url = path ++ query
      uri = fromJust $ U.parseURI url
      req = H.Request { H.rqURI = uri,
                        H.rqMethod = methodStringToData method,
                        H.rqHeaders = heads,
                        H.rqBody = "" }
  in do resp <- H.simpleHTTP req
        case resp of
          Left x -> return $ Left ("Error connecting: " ++ show x)
          Right r ->
            case H.rspCode r of
              (2,_,_) -> return $ Right (H.rspBody r)
              _       -> return $ Left (show r)

-- argsToPath :: RequestTemplate Args String -> (Either String String)
-- argsToPath rt args endpoint

foo :: String -> (Either String String)
foo input = do
  x <- m input
  y <- m input
  return y

m :: String -> (Either String String)
m "good" = return "good"
m "bad" = Left "error"



methodStringToData s =
  case s of
    "GET" -> H.GET
    "PUT" -> H.PUT
    "POST" -> H.POST
    "DELETE" -> H.DELETE
    "OPTIONS" -> H.OPTIONS
    s -> error $ "Unknown HTTP method " ++ show s

-- Utility stuff

gatherBy :: (a -> Bool) -> [a] -> [[a]]
gatherBy pred [] = []
gatherBy pred (x:xs) =
  let run  = takeWhile (not . pred) xs
      more = dropWhile (not . pred) xs
  in if pred x
       then (x : run) : (gatherBy pred more)
       else []

-- Example usage:
test = do
  let md = foldl1 (++) $ intersperse "\n"
           ["# The Foo Service",
            "Para of intro text.",
            "",
            "Endpoint: http://horseebooksipsum.com",
            "",
            "# Get Stuff",
            "",
            "## Request",
            "",
            "```",
            "GET /api/v1/{paragraphs}",
            "Server: {}",
            "Header: {Aliased-Header}",
            "```",
            "",
            ""]
  print md
  let service = markdownToService md
  print service
  -- Get the name of the function
  let name = apiName $ head $ functions service
  -- TO-DO camelCase this -- e.g. "Get Stuff" => "getStuff" to form
  -- valid Haskell identifier.
  print name
  let f = request $ head $ functions service

  -- TO-DO: Use Template Haskell to define a function named `name`
  -- that wraps the request function.

  result <- f [("paragraphs","2")
              ,("alias","foo")
              ,("Server","bar")
              ,("Aliased-Header","2")
              ]
  print result
