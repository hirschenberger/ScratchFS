{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-
  ScratchFs is a size limited temp filesystem based on fuse
  Copyright (C) 2012  Falco Hirschenberger <hirsch@bogfoot.de>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module HistoryDb (historyDb, 
                  addFile,
                  deleteOldestFile,
                  sizeOfDbFiles,
                  Connection,
                  HistoryField) where

import Data.Maybe
import System.Process
import System.FilePath.Posix
import System.Posix
import System.Directory
import System.Posix.Syslog
import Control.Monad
import Control.Applicative
import qualified Control.Exception as CE
import Database.SQLite.Simple


type Time = Int
type Size = Integer

data HistoryField = HistoryField Int Time Size FilePath deriving (Show)

instance FromRow HistoryField where
    fromRow = HistoryField <$> field <*> field <*> field <*> field

dbFileName:: String
dbFileName = "scratchfs.db"

createDbArgs:: [String]
createDbArgs = [dbFileName, 
                "CREATE TABLE IF NOT EXISTS scratchfs \
                \(id INTEGER PRIMARY KEY, \
                \time INTEGER, size INTEGER, path TEXT);"]

{-|
  Create the history database in the root-path of the filesystem if it does not exist already.
  If it exists, the db will be opened and it's connection handle returned

  TODO: Perhaps embedding a precreated database file in the executable with the
      file-embed module is a better approach than calling external programs
-}
historyDb:: FilePath        -- ^ Path to the base directory of the filesystem.
         -> IO Connection   -- ^ The open database connection
historyDb path =
    runProcess "sqlite3" createDbArgs (Just path) Nothing Nothing Nothing Nothing >>= 
    waitForProcess >>
    open (path </> dbFileName)

{-|
  Insert a file to the database, the size and time gets added automatically to the database.

  Note: The time that is used is the current epoch-time, not the file's timestamp.
-}
addFile:: Connection        -- ^ The open database connection
       -> FilePath          -- ^ The file to add to the database
       -> IO (Time, Size)   -- ^ Return the time and size of the added file
addFile conn path = do
    time <- liftM fromEnum epochTime
    size <- liftM (fromIntegral.fileSize) (getSymbolicLinkStatus path)
    execute conn "INSERT INTO scratchfs (time, size, path) VALUES (?, ?, ?)" 
                 (time, size :: Integer, path)
    return (time, size)                 

{-|
  Delete the oldest file from the database and from harddisk. If the file doesn't exist,
  the entry gets removed from the database and the exception is ignored, but logged.
-}
deleteOldestFile:: Connection  -- ^ The open database connection        
                -> IO Integer  -- ^ The size of the deleted file in bytes
deleteOldestFile conn = do
    rep <- query_ conn "SELECT * FROM scratchfs ORDER BY time LIMIT 1" :: IO [HistoryField]
    case rep of
        []                                 -> return 0
        (HistoryField key  _  sz  path):_  -> delFile path >> delKey key >> return sz
    where
      delFile:: FilePath -> IO ()
      delFile p = removeFile p `CE.catch` (\(e :: CE.SomeException) -> 
                            syslog Debug ("Delete file: " ++ p ++ " exception: " ++ show e))
      delKey:: Int -> IO ()                  
      delKey k  = execute conn "DELETE FROM scratchfs WHERE id IS ?" (Only k)

{-|
  The size of all files that are in the database. Note that these files do not have to exist
  on disk, so this size may be bigger than the size of the files on disk.
-}
sizeOfDbFiles:: Connection -- ^ The open database connection
             -> IO Integer -- ^ The sum of the sizes of all files in the database
sizeOfDbFiles conn = do
    res <- query_ conn "SELECT sum(size) FROM scratchfs"
    case res of
      [(Only (Just size))] -> return size
      otherwise     -> return 0
