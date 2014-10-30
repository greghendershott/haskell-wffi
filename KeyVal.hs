module KeyVal(
  KeyVal(..),
  Value(..)
) where

data KeyVal = KeyVal { key :: String -- name to use in the request
                     , val :: Value  -- value to use in the request
                     } deriving (Show)

data Value = Variable (Maybe String) -- alias in request code
           | Constant String
           | Optional Value
           deriving (Show)
