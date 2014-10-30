module Wffi.KeyVal(
  KeyVal(..),
  Value(..)
) where

data KeyVal = KeyVal { key :: String -- name to use in the request
                     , val :: Value  -- value to use in the request
                     } deriving (Show)

data Value = Variable { varAlias :: (Maybe String) -- alias in request code
                      , varVal :: String }         -- value to use in req
           | Constant String
           | Optional Value
           deriving (Show)
