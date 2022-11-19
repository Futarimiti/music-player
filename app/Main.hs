{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative    (Alternative)
import           Control.Monad          (guard)
import           Data
import           Data.Maybe             (fromMaybe, isJust, isNothing)
import           Database.HDBC          (IConnection (disconnect, prepare),
                                         Statement (execute), commit, fromSql,
                                         quickQuery', toSql)
import           Database.HDBC.SqlValue (SqlValue (..))
import           DoIO                   (askWhatToAdd, errNoSuchCmd, prompt,
                                         putHelp)
import           ProjConfig             (musicDBConn)
import           System.Environment     (getArgs)
import           Text.Printf            (printf)
import           Util

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

doAddGenre :: IO ()
doAddGenre = do genreName <- prompt "Genre name: "
                reqNonEmpty genreName
                let genre = Genre genreName
                conn <- musicDBConn
                addGenre genre conn
                commit conn
                disconnect conn

doAddAlbum :: IO ()
doAddAlbum = do albumName <- prompt "Name of album: "
                reqNonEmpty albumName
                let album = Album albumName
                conn <- musicDBConn
                addAlbum album conn
                commit conn
                disconnect conn

doAddArtist :: IO ()
doAddArtist = do artistName <- prompt "Name of artist: "
                 reqNonEmpty artistName
                 let artist = Artist { primaryName = artistName }
                 conn <- musicDBConn
                 addArtist artist conn
                 commit conn
                 disconnect conn

doAddSong :: IO ()
doAddSong = do songName <- prompt "Name of song: "
               reqNonEmpty songName
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
               reArtistID <- if artistID == "" then Just <$> prompt "What's the ID now? " else return Nothing
               guard $ isNothing reArtistID && artistID /= "" || isJust reArtistID && reArtistID /= Just "" && artistID == ""
               artistDisp <- prompt "Name of artist in display: "
               reqNonEmpty artistDisp
               sourceURL <- prompt "[Optional] URL link to the source: "
               filename <- prompt "[Optional] Desired filename for the audio file: "
               let song = Song { songName=songName, artistDisp=artistDisp, sourceURL=strToMaybe sourceURL, filename=strToMaybe filename }
               conn <- musicDBConn
               addSong song conn
               commit conn
               disconnect conn
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

addAlbum :: IConnection conn => Album -> conn -> IO ()
addAlbum (Album {..}) conn = do stmt <- prepare conn "INSERT INTO album (album_name) VALUES (?)"
                                execute stmt [toSql albumName]
                                return ()

addArtist :: IConnection conn => Artist -> conn -> IO ()
addArtist (Artist {..}) conn = do stmt <- prepare conn "INSERT INTO artist (artist_name) VALUES (?)"
                                  execute stmt [toSql primaryName]
                                  return ()

addGenre :: IConnection conn => Genre -> conn -> IO ()
addGenre (Genre {..}) conn = do stmt <- prepare conn "INSERT INTO genre (genre_name) VALUES (?)"
                                execute stmt [toSql genreName]
                                return ()
