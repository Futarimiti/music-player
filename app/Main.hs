{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data
import           Data.Maybe             (fromMaybe)
import           Database.HDBC          (IConnection (disconnect, prepare),
                                         Statement (execute), commit, fromSql,
                                         quickQuery', toSql)
import           Database.HDBC.SqlValue (SqlValue (..))
import           DoIO                   (askWhatToAdd, errNoSuchCmd, prompt,
                                         putHelp)
import           ProjConfig             (musicDBConn)
import           System.Environment     (getArgs)
import           Text.Printf            (printf)

main :: IO ()
main = do args <- getArgs
          processCmd args

processCmd :: [String] -> IO ()
processCmd = \case []         -> putHelp
                   ["new"] -> do x <- askWhatToAdd
                                 addX x
                   ["new", x] -> addX x
                   _          -> errNoSuchCmd >> putHelp

addX :: String -> IO ()
addX = \case "song"   -> doAddSong
             "artist" -> doAddArtist
             "album"  -> doAddAlbum
             _        -> errNoSuchCmd

doAddAlbum :: IO ()
doAddAlbum = undefined

doAddArtist :: IO ()
doAddArtist = undefined

doAddSong :: IO ()
doAddSong = do songName <- prompt "Name of song: "
               artistID <- prompt "ID of the artist: "
               showArtistTable <- if artistID == "" then Just <$> prompt "Show artist table? [y/n] " else return Nothing
               case showArtistTable of
                 Just "y" -> doShowArtistTable
                 Just "Y" -> doShowArtistTable
                 Just "" -> doShowArtistTable
                 Just "n" -> return ()
                 Just "N" -> return ()
                 Just _ -> putStrLn "I cannot read this but I guess you want to decline."
                 Nothing -> return ()
               reArtistID <- prompt "What's the ID now? "
               artistDisp <- prompt "Name of artist in display: "
               putStrLn (printf "Added new song: '%s' by %s" songName artistDisp)
               return ()

doShowArtistTable :: IO ()
doShowArtistTable = do conn <- musicDBConn
                       rs <- quickQuery' conn "SELECT artist.id, artist.primary_name FROM artist" []
                       let res = map (\[id, name] -> printf "%d\t%s" (fromSql id :: Int) (fromSql name :: String)) rs
                       let str = unlines res
                       putStrLn str
                       disconnect conn

-- add a new song in form of a Song struct to database
-- does not commit nor disconnect conn at the end - rollbackable
addSong :: IConnection conn => Song -> conn -> IO ()
addSong (Song {..}) conn = do stmt <- prepare conn "INSERT INTO song (song_name, artist_disp, source_url, filename) VALUES (?, ?, ?, ?)"
                              execute stmt [toSql songName, toSql artistDisp, maybe SqlNull toSql sourceURL, maybe SqlNull toSql filename]
                              return ()

