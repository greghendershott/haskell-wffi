module Wffi(
  markdownToService
) where

import Data.Set (insert, delete)
import Data.List (intersperse)
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Show.Functions -- to Show function member of ApiFunction
import qualified Network.HTTP as H
import ParseRequest
import KeyVal

type Args = [(String, String)]

-- FIXME: Convert the [Block] and [Inline] to String using some Pandoc
-- functionality?

data ApiFunction = ApiFunction { apiName :: [Inline]
                               , apiDescription :: [Block]
                               , requestTemplateRaw :: String -- for debugging
                               , requestTemplate :: Maybe RequestTemplate
                               , request :: Args -> String
                               } deriving (Show)

data Service = Service { serviceName :: [Inline]
                       , serviceDescription :: [Block]
                       , endpoint :: String
                       , functions :: [ApiFunction]
                       } deriving (Show)

markdownToService :: String -> Service
markdownToService s =
  let pd = markdownToPandoc s
      sections = h1Sections pd
      intro = head sections
      name = case intro of ((Header 1 _ x) : _) -> x
      desc = case intro of ((Header 1 _ _) : x) -> x
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
  let name = case blocks of ((Header 1 _ x) : _) -> x
      desc = case blocks of ((Header 1 _ _) : x) -> x
      rawrt = findRequestTemplate blocks
      rt = parseRequestTemplate rawrt
      f = case rt of
        (Just t) -> requestTemplateToWrapper t endpoint
        Nothing  -> \_ -> "could not parse request template"
  in ApiFunction{ apiName = name,
                  apiDescription = desc,
                  requestTemplateRaw = rawrt,
                  requestTemplate = rt,
                  request = f }

findRequestTemplate :: [Block] -> String
findRequestTemplate [] = "not-found"
findRequestTemplate ((CodeBlock _ s) : _) = s
findRequestTemplate (_:xs) = findRequestTemplate xs

requestTemplateToWrapper :: RequestTemplate -> String -> (Args -> String)
requestTemplateToWrapper rt endpoint = doRequest rt endpoint

doRequest :: RequestTemplate -> String -> Args -> String
doRequest rt endpoint args =
  let method = rtMethod rt
      pathValue x =
        case x of
          (Constant s)        -> s
          (Variable (Just k)) -> (case lookup k args of
                                     (Just s) -> s
                                     _        -> error "path error")
          _                   -> error "path error"
      paths = (map pathValue (rtPathParts rt))
      path = endpoint ++ "/" ++ (foldl1 (++) (intersperse "/" paths))
      -- Some code that can be shared among query params and headers
      paramValue e fk x =
        case x of
          (KeyVal k (Constant v))        -> Just $ k ++ e ++ v
          (KeyVal _ (Variable (Just k))) -> (case lookup k args of
                                                (Just v) -> Just $ k ++ e ++ v
                                                _        -> fk k)
          (KeyVal k (Variable Nothing))  -> (case lookup  k args of
                                                (Just v) -> Just $ k ++ e ++ v
                                                _        -> fk k)
          (KeyVal k (Optional v)) -> paramValue e (\_ -> Nothing) (KeyVal k v)
      missing what name =
        error $ "Missing required " ++ what ++ ": `" ++ name ++ "'"
      -- The following 2 lines seem like a code smell... ?
      something x = case x of Nothing -> False; _ -> True
      just (Just x) = x
      -- Query parameters
      queries = map (paramValue "=" (missing "query parameter"))
                (rtQueryParams rt)
      query = case (map just (filter something queries)) of
        [] -> ""
        xs -> "?" ++ (foldl1 (++) (intersperse "&" xs))
      -- Headers
      heads = map (paramValue ":" (missing "header"))
              (rtHeaders rt)
      head = case (map just (filter something heads)) of
        [] -> ""
        xs -> "\n" ++ (foldl1 (++) (intersperse "\n" xs)) ++ "\n\n"
  in method ++ " " ++ path ++ query ++ head

-- Utility stuff

gatherBy :: (a -> Bool) -> [a] -> [[a]]
gatherBy pred [] = []
gatherBy pred (x:xs) =
  let run = takeWhile (complement pred) xs
      more = dropWhile (complement pred) xs
  in (x : run) : (gatherBy pred more)

complement :: (a -> Bool) -> (a -> Bool)
complement f x = not $ f x

-- Example usage:
test = do
  let md = foldl1 (++) $ intersperse "\n"
           ["# The Foo Service",
            "Para of intro text.",
            "",
            "Endpoint: http://foo.bar.com/api/v1",
            "",
            "# Get",
            "",
            "## Request",
            "",
            "GET /users/{user}?qp0={}&[qpOpt={}]&qpAlias={alias}",
            "Server: {}",
            "Header: {Aliased-Header}",
            "```",
            "",
            ""]
  print md
  let service = markdownToService md
  print service
  let function = head $ functions service
  let f = request function
  print $ f [("user","Greg"),
             ("qp0","qpval0"),
             ("alias","foo"),
             ("Server","bar")]

