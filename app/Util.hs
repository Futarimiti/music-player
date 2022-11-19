module Util where

reqNonEmpty :: Alternative m => [b] -> m ()
reqNonEmpty = guard . not . null

strToMaybe :: String -> Maybe String
strToMaybe "" = Nothing
strToMaybe bs = Just bs

