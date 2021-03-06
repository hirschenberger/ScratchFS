{-# LANGUAGE NamedFieldPuns #-}
{-
  ScratchFs is a size limited temp filesystem based on fuse
  Copyright (C) 2012  Falco Hirschenberger <hirsch@bigfoot.de>

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

module Main where

import           Debug.Trace
import           Control.Exception
import           Control.Monad
import           Control.Concurrent.QSem
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Text.Printf
import           System.Console.GetOpt
import           System.Directory      (getDirectoryContents)
import           System.Environment
import           System.Exit
import           System.Fuse
import           System.IO
import           System.CPUTime
import           System.Posix
import           System.Posix.Syslog
import           System.FilePath.Posix

import           Utils
import           HistoryDb

data Options = Options { maxSize :: Integer }

data State = State { dbConn  :: Connection
                    ,opts    :: Options
                    ,qsem    :: QSem }

defaultOptions:: Options
defaultOptions = Options { maxSize = 0 }

options:: [OptDescr (Options -> IO Options)]
options = [ Option "s" ["size"]
                    (ReqArg (\arg opt -> return opt {maxSize = fromJust (parseSize arg)}) "SIZE")
                    "The maximum size in kilobytes (allowed extensions: MB, GB, TB)"
          , Option "h"  ["help"] (NoArg (printHelp ExitSuccess)) "Show help message"
          ]

printHelp:: ExitCode -> Options -> IO Options
printHelp c _ = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo (prg ++ " {-s|--size} SIZE WATCHDIR MOUNTDIR") options)
    exitWith c

main:: IO ()
main = withSyslog "ScratchFS" [PID, PERROR] USER $ do
        args <- getArgs
        when (length args /= 4) $ void $ printHelp (ExitFailure 1) defaultOptions
        case getOpt RequireOrder options args of
            (o, [watchDir, mountDir], []) -> do
                opts    <- foldl (>>=) (return defaultOptions) o
                dbConn  <- historyDb watchDir
                qsem <- newQSem 1
                syslog Debug ("Starting ScratchFS from " ++ watchDir ++ " mounted on " ++ mountDir)
                withArgs ["-f", mountDir] $ 
                      fuseMain (scratchOps watchDir State{ dbConn, opts, qsem }) exceptionHandler
            (_, _, e)  -> void (putStrLn (concat e) >> printHelp (ExitFailure 1) defaultOptions)

exceptionHandler:: SomeException -> IO Errno
exceptionHandler e = syslog Error ("Exception: " ++ show e) >> defaultExceptionHandler e

scratchOps:: FilePath -> State -> FuseOperations Fd
scratchOps root state = defaultFuseOps {fuseGetFileStat         = scratchGetFileStat root,
                                        fuseCreateDevice        = scratchCreateDevice root,
                                        fuseRemoveLink          = scratchRemoveLink root,
                                        fuseCreateDirectory     = scratchCreateDirectory root,
                                        fuseRemoveDirectory     = scratchRemoveDirectory root ,
                                        fuseOpenDirectory       = scratchOpenDirectory root,
                                        fuseReadDirectory       = scratchReadDirectory root,
                                        fuseRename              = scratchRename root,
                                        fuseSetFileMode         = scratchSetFileMode root,
                                        fuseSetOwnerAndGroup    = scratchSetOwnerAndGroup root,
                                        fuseSetFileSize         = scratchSetFileSize root,
                                        fuseSetFileTimes        = scratchSetFileTimes root,
                                        fuseOpen                = scratchOpen root,
                                        fuseWrite               = scratchWrite root,
                                        fuseRead                = scratchRead root,
                                        fuseGetFileSystemStats  = scratchGetFileSystemStats,
                                        fuseFlush               = scratchFlush,
                                        fuseRelease             = scratchRelease root state,
                                        fuseSynchronizeFile     = scratchSynchronizeFile
                                        }


fileStatusToFileStat :: FileStatus -> FileStat
fileStatusToFileStat status =
    FileStat { statEntryType        = (fileModeToEntryType.fileMode) status
             , statFileMode         = fileMode status
             , statLinkCount        = linkCount status
             , statFileOwner        = fileOwner status
             , statFileGroup        = fileGroup status
             , statSpecialDeviceID  = specialDeviceID status
             , statFileSize         = fileSize status
             -- fixme: 1024 is not always the size of a block
             , statBlocks           = fromIntegral (fileSize status `div` 512)
             , statAccessTime       = accessTime status
             , statModificationTime = modificationTime status
             , statStatusChangeTime = statusChangeTime status
             }

(<//>):: FilePath -> FilePath -> FilePath
(<//>) a b = normalise (a ++ "/" ++ b)

scratchCreateDevice:: FilePath -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
scratchCreateDevice r p t m d = do 
    traceIO $ "Create device: " ++ (r <//> p)
    let combinedMode = entryTypeToFileMode t `unionFileModes` m
    createDevice (r <//> p) combinedMode d
    getErrno

scratchRemoveLink:: FilePath -> FilePath -> IO Errno
scratchRemoveLink r p = removeLink (r <//> p) >> getErrno

scratchGetFileStat:: FilePath -> FilePath -> IO (Either Errno FileStat)
scratchGetFileStat r s = do
    exist <- fileExist path
    if exist
      then do stat <- getSymbolicLinkStatus path
              return $ Right $ fileStatusToFileStat stat
      else    return $ Left eNOENT
    where
      path = r <//> s

scratchCreateDirectory:: FilePath -> FilePath -> FileMode -> IO Errno
scratchCreateDirectory r p m = createDirectory (r <//> p) m >> getErrno

scratchRemoveDirectory:: FilePath -> FilePath -> IO Errno
scratchRemoveDirectory r p = removeDirectory (r <//> p) >> getErrno

scratchOpenDirectory:: FilePath -> FilePath -> IO Errno
scratchOpenDirectory r p = openDirStream (r <//> p) >>= closeDirStream >> getErrno

scratchReadDirectory :: FilePath -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
scratchReadDirectory r p = liftM Right (getDirectoryContents (r <//> p) >>= mapM pairType)
    where pairType name = do
              status <- getSymbolicLinkStatus (r <//> name)
              return (name, fileStatusToFileStat status)

scratchRename :: FilePath -> FilePath -> FilePath -> IO Errno
scratchRename root src dest = rename (root <//> src) (root <//> dest) >> getErrno

scratchSetFileMode :: FilePath -> FilePath -> FileMode -> IO Errno
scratchSetFileMode root s mode = setFileMode (root <//> s) mode >> getErrno

scratchSetOwnerAndGroup :: FilePath -> FilePath -> UserID -> GroupID -> IO Errno
scratchSetOwnerAndGroup root path uid gid = setOwnerAndGroup (root <//> path) uid gid  >> getErrno

scratchSetFileSize :: FilePath -> FilePath -> FileOffset -> IO Errno
scratchSetFileSize root path off = setFileSize (root <//> path) off >> getErrno

scratchSetFileTimes :: FilePath -> FilePath -> EpochTime -> EpochTime -> IO Errno
scratchSetFileTimes root s aTime mTime = setFileTimes (root <//> s) aTime mTime >> getErrno

scratchOpen :: FilePath -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Fd)
scratchOpen r p mode flags = liftM Right (openFd (r <//> p) mode (Just stdFileMode) flags)

scratchRead :: FilePath -> FilePath -> Fd -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
scratchRead _ _ fd count off = do
    bs <- withBinaryHandle B.hGet fd (fromIntegral count) off
    return.Right $ bs

scratchWrite :: FilePath -> FilePath -> Fd -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
scratchWrite _ _ fd buf off = do
    withBinaryHandle B.hPut fd buf off
    return.Right $ fromIntegral $ B.length buf

scratchGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
scratchGetFileSystemStats _ = return $ Left eOK

scratchFlush :: FilePath -> Fd -> IO Errno
scratchFlush _ _ = getErrno

-- protect the cleanup function with a semaphore to avoid concurrent access to the database
scratchRelease :: FilePath -> State -> FilePath -> Fd -> IO ()
scratchRelease r State{dbConn, opts, qsem} p fd = do    
    waitQSem qsem
    start <- getCPUTime
    _ <- addFile dbConn path
    newSz <- sizeOfDbFiles dbConn
    maybeCleanUp newSz
    closeFd fd
    stop <- getCPUTime
    let diff = fromIntegral (stop - start) / (10^(12::Integer))
    traceIO $ printf "Time: %0.3f sec" (diff :: Double)
    signalQSem qsem
    where
      path:: FilePath
      path = r <//> p
      maybeCleanUp:: Integer -> IO ()
      maybeCleanUp size
          | size > maxSize opts = do
                                  traceIO (printf "Cleanup, size: %d" size)
                                  sd <- deleteOldestFile dbConn
                                  maybeCleanUp (size - sd)
          | otherwise           = return ()                             


scratchSynchronizeFile :: FilePath -> SyncType -> IO Errno
scratchSynchronizeFile _ _ = getErrno

-- Some stupid Fd -> Handle conversion is done here because the fd-based IO operations
-- perform some encoding-magic on the data which we don't need here. We need plain binary
-- date writes
withBinaryHandle:: (Handle -> a -> IO b) -> Fd -> a -> FileOffset -> IO b
withBinaryHandle f fd a fo = do
    hd <- fdToHandle fd
    hSeek hd AbsoluteSeek $ fromIntegral fo
    res <- f hd a
    _ <- handleToFd hd
    return res
