module Util where

import Control.Applicative (Alternative)
import Control.Monad (guard)

reqNonEmpty :: Alternative m => [b] -> m ()
reqNonEmpty = guard . not . null

strToMaybe :: String -> Maybe String
strToMaybe "" = Nothing
strToMaybe bs = Just bs

