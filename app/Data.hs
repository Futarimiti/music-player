module Data where

data Song = Song { songName   :: String
                 , artistDisp :: String
                 , sourceURL  :: Maybe String
                 , filename   :: Maybe String
                 } deriving (Show, Eq, Read)

newtype Artist = Artist { primaryName :: String }
  deriving (Show, Eq, Read)

newtype Genre = Genre { genreName :: String }
  deriving (Show, Eq, Read)

newtype Album = Album { albumName :: String }
  deriving (Show, Eq, Read)

hemisphere = Song { songName = "Hemisphere"
                  , artistDisp = "ARForest"
                  , sourceURL = Just "https://soundcloud.com/arforest/in-dynamixhemisphere"
                  , filename = Just "hemisphere.mp3"
                  }
