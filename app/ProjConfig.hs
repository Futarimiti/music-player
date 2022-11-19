module ProjConfig where

import           Database.HDBC.Sqlite3 (Connection, connectSqlite3)

musicDBConn :: IO Connection
musicDBConn = connectSqlite3 "app/resources/musicdb.db"

cmdName :: String
cmdName = "music-player"
