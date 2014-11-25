{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Template where

import Data.Char (toLower, toUpper)
import Language.Haskell.TH
import Language.Haskell.TH.Lift

import KeyVal
import ParseRequest
import Wffi

$(deriveLift ''Value)
$(deriveLift ''KeyVal)
$(deriveLift ''RequestTemplate)

mkService :: Service -> Q [Dec]
mkService srv = mapM getFunction (functions srv)
  where servicePrefix = serviceName srv

        capitalize (c:cs) = toUpper c : cs
        getFunctionName fn = mkName $
            (map toLower servicePrefix) ++ capitalize (apiName fn)

        getFunction :: ApiFunction -> Q Dec
        getFunction fn = funD (getFunctionName fn) [b]
          where e = endpoint srv
                Just t = requestTemplate fn
                b = clause []
                           (normalB (mkDoRequest t e))
                           []

        mkDoRequest :: RequestTemplate -> String -> Q Exp
        mkDoRequest t e = [| doRequest t e |]
