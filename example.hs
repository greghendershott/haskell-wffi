{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Main
    (main)
  where

import ParseRequest
import Template
import Wffi

$(do
    let googleApi :: Service
        googleApi = Service { serviceName = "Google"
                            , serviceDescription = "Google is a search engine"
                            , endpoint = "https://google.com/"
                            , functions = [
                              ApiFunction { apiName = "search"
                                          , apiDescription = undefined
                                          , requestTemplateRaw = undefined
                                          , requestTemplate = Just $ RequestTemplate
                                                { rtMethod = "GET"
                                                , rtPathParts = []
                                                , rtQueryParams = []
                                                , rtHeaders = []
                                                , rtBody = ""
                                                }
                                          , request = undefined
                                          }
                              ]
                            }
    mkService googleApi)

main :: IO ()
main = undefined
