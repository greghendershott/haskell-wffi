module Wffi(
--  markdownToService
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intersperse)
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Show.Functions -- b/c function member of ApiFunction
import ParseRequest

type RequestMap = Map.Map String String

-- FIXME: Convert the [Block] and [Inline] to String using some Pandoc
-- functionality?

data ApiFunction = ApiFunction { apiName :: [Inline]
                               , apiDescription :: [Block]
                               , requestTemplateRaw :: String -- for debugging
                               , requestTemplate :: Maybe RequestTemplate
                               , request :: RequestMap -> String
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
               functions = map sectionToApiFunction $ tail sections }

markdownToPandoc :: String -> Pandoc
markdownToPandoc s = Text.Pandoc.Readers.Markdown.readMarkdown
                     -- Is there a less-verbose way to do the following?
                     (Text.Pandoc.Options.ReaderOptions
                      Set.empty
                      False
                      False
                      False
                      80
                      4
                      False
                      False
                      []
                      ".png"
                      False
                      Text.Pandoc.Options.AllChanges)
                     s

h1Sections :: Pandoc -> [[Block]]
h1Sections (Pandoc _ blocks) = gatherBy h1Pred blocks

h1Pred :: Block -> Bool
h1Pred (Header 1 _ _) = True
h1Pred  _             = False

findEndpoint :: [Block] -> String
findEndpoint [] = "not-found"
findEndpoint ((Para [Str "Endpoint:", Space, Str url]) : _) = url
findEndpoint (_ : xs) = findEndpoint xs

sectionToApiFunction :: [Block] -> ApiFunction
sectionToApiFunction blocks =
  let name = case blocks of ((Header 1 _ x) : _) -> x
      desc = case blocks of ((Header 1 _ _) : x) -> x
      rawrt = findRequestTemplate blocks
      rt = parseRequestTemplate rawrt
      f = case rt of
        (Just t) -> requestTemplateToWrapper t
        Nothing  -> \_ -> "could not parse request template"
  in ApiFunction{ apiName = name,
                  apiDescription = desc,
                  requestTemplateRaw = rawrt,
                  requestTemplate = rt,
                  request = f }

findRequestTemplate :: [Block] -> String
findRequestTemplate [] = "not-found"
findRequestTemplate ((Para [(Code _ s)]) : _) = s
findRequestTemplate (_:xs) = findRequestTemplate xs

requestTemplateToWrapper :: RequestTemplate -> (RequestMap -> String)
requestTemplateToWrapper _ = (\_ -> "TO-DO")

-- Utility stuff

gatherBy :: (a -> Bool) -> [a] -> [[a]]
gatherBy pred [] = []
gatherBy pred (x:xs) = let run = takeWhile (complement pred) xs
                           more = dropWhile (complement pred) xs in
  (x : run) : (gatherBy pred more)

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
            "```",
            "GET /users/{user}",
            "```",
            ""]
  print $ markdownToService md
