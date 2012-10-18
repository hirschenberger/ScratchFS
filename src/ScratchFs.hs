{-# LANGUAGE NamedFieldPuns #-}
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

module Main where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Data.IORef
import           Text.Printf
import           System.Console.GetOpt
import           System.Directory      (getDirectoryContents)
import           System.Environment
import           System.Exit
import           System.Fuse
import           System.IO
import           System.Posix
import           System.Posix.Syslog
import           System.FilePath.Posix

import           Utils
import           HistoryDb

data Options = Options { maxSize :: Integer }

data State = State { dbConn  :: Connection
                    ,size    :: Integer
                    ,opts    :: Options }

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
    hPutStrLn stderr (usageInfo (prg ++ " [OPTIONS] WATCHDIR MOUNTDIR") options)
    exitWith c

main:: IO ()
main = withSyslog "ScratchFS" [PID, PERROR] USER $ do
        args <- getArgs
        case getOpt RequireOrder options args of
            (o, [watchDir, mountDir], []) -> do
                opts    <- foldl (>>=) (return defaultOptions) o
                dbConn  <- historyDb watchDir
                size    <- sizeOfDbFiles dbConn
                state   <- newIORef $ State{ dbConn, size, opts}
                syslog Debug ("Starting ScratchFS from " ++ watchDir ++ " mounted on " ++ mountDir)
                withArgs ["-f", mountDir] $ fuseMain (scratchOps watchDir state) exceptionHandler
            (_, _, e)  -> void (putStrLn (concat e) >> printHelp (ExitFailure 1) defaultOptions)

exceptionHandler:: SomeException -> IO Errno
exceptionHandler e = syslog Error ("Exception: " ++ show e) >> defaultExceptionHandler e

scratchOps:: FilePath -> IORef State -> FuseOperations Fd
scratchOps root state = defaultFuseOps {fuseGetFileStat         = scratchGetFileStat root,
                                        fuseCreateDevice        = scratchCreateDevice root,
                                        fuseRemoveLink          = scratchRemoveLink root,
                                        fuseCreateDirectory     = scratchCreateDirectory root,
                                        fuseRemoveDirectory     = scratchRemoveDirectory root ,
                                        fuseOpenDirectory       = scratchOpenDirectory root,
                                        fuseReadDirectory       = scratchReadDirectory root,
                                        fuseRename              = scratchRename,
                                        fuseSetFileMode         = scratchSetFileMode,
                                        fuseSetOwnerAndGroup    = scratchSetOwnerAndGroup,
                                        fuseSetFileSize         = scratchSetFileSize,
                                        fuseSetFileTimes        = scratchSetFileTimes,
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
    syslog Debug $ "Create device: " ++ (r <//> p)
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

scratchRename :: FilePath -> FilePath -> IO Errno
scratchRename src dest = rename src dest >> getErrno

scratchSetFileMode :: FilePath -> FileMode -> IO Errno
scratchSetFileMode path mode = setFileMode path mode >> getErrno

scratchSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno
scratchSetOwnerAndGroup path uid gid = setOwnerAndGroup path uid gid  >> getErrno

scratchSetFileSize :: FilePath -> FileOffset -> IO Errno
scratchSetFileSize path off = setFileSize path off >> getErrno

scratchSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno
scratchSetFileTimes path aTime mTime = setFileTimes path aTime mTime >> getErrno

scratchOpen :: FilePath -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Fd)
scratchOpen r p mode flags = liftM Right (openFd (r <//> p) mode (Just stdFileMode) flags)

scratchRead :: FilePath -> FilePath -> Fd -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
scratchRead _ _ fd count off = do
    newOff <- fdSeek fd AbsoluteSeek off
    if off /= newOff
        then return (Left eINVAL)
        else do (content, _) <- fdRead fd count
                return.Right $ B.pack content

scratchWrite :: FilePath -> FilePath -> Fd -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
scratchWrite _ _ fd buf off = do
    newOff <- fdSeek fd AbsoluteSeek off
    if off /= newOff
        then return (Left eINVAL)
        else do
            _ <- fdWrite fd (B.unpack buf)         
            return.Right $ fromIntegral $ B.length buf

scratchGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
scratchGetFileSystemStats _ = return (Left eOK)

scratchFlush :: FilePath -> Fd -> IO Errno
scratchFlush _ _ = getErrno

scratchRelease :: FilePath -> IORef State -> FilePath -> Fd -> IO ()
scratchRelease r stRef p fd = do
    state@State{dbConn, size} <- readIORef stRef
    (_, fileSz) <- addFile dbConn path
    maybeCleanUp state{size = size + fileSz} >>= writeIORef stRef 
    closeFd fd
    where
      path:: FilePath
      path = r <//> p
      mustClean:: State -> Bool
      mustClean State{opts, size} = size  > maxSize opts
      maybeCleanUp:: State -> IO (State)
      maybeCleanUp st@State{size, dbConn}
          | mustClean st = do
                            syslog Debug (printf "Cleanup, size: %d" size)
                            sd <- deleteOldestFile dbConn
                            maybeCleanUp st {size = size - sd}
          | otherwise    =  return st                             


scratchSynchronizeFile :: FilePath -> SyncType -> IO Errno
scratchSynchronizeFile _ _ = getErrno
