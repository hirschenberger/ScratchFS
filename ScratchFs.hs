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
import qualified Data.IntMap           as IM
import           Data.Maybe
import           Data.IORef
import           System.Console.GetOpt
import           System.Directory      (getDirectoryContents)
import           System.Environment
import           System.Exit
import           System.FilePath.Posix ((</>))
import           System.Fuse
import           System.IO
import           System.Posix
import           System.Posix.Syslog
import           System.FilePath.Posix
import           Utils

data Options = Options { maxSize :: Int }

data State = State { history :: IM.IntMap FilePath
                    ,size    :: Int }

newState:: State 
newState = State IM.empty 0

defaultOptions:: Options
defaultOptions = Options 0

options:: [OptDescr (Options -> IO Options)]
options = [ Option "s" ["size"]
                    (ReqArg (\arg opt -> return opt {maxSize = fromJust (parseSize arg)}) "SIZE")
                    "The maximum size in kilobytes (allowed extensions: MB, GB, TB)"
          , Option "h"  ["help"] (NoArg (printHelp ExitSuccess)) "Show help message"
          ]

printHelp:: ExitCode -> Options -> IO (Options)
printHelp c _ = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo (prg ++ " [OPTIONS] WATCHDIR MOUNTDIR") options)
    exitWith c

main:: IO ()
main = withSyslog "ScratchFS" [PID, PERROR] USER $ do
        args <- getArgs
        case getOpt RequireOrder options args of
            (o, [watchDir, mountDir], []) -> do
                opts <- foldl (>>=) (return defaultOptions) o
                state <- newIORef newState
                syslog Debug ("Starting ScratchFS from " ++ watchDir ++ " mounted on " ++ mountDir)
                withArgs ["-f", "-d", mountDir] $ fuseMain (scratchOps watchDir state) exceptionHandler
            (_, _, e)  -> putStrLn (concat e) >> printHelp (ExitFailure 1) defaultOptions >> return ()

exceptionHandler:: SomeException -> IO Errno
exceptionHandler e = syslog Error ("Exception: " ++ show e) >> defaultExceptionHandler e

scratchOps:: FilePath -> IORef State -> FuseOperations Fd
scratchOps root state = defaultFuseOps {fuseGetFileStat         = scratchGetFileStat root,
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
                                        fuseWrite               = scratchWrite root state,
                                        fuseRead                = scratchRead root,
                                        fuseGetFileSystemStats  = scratchGetFileSystemStats,
                                        fuseFlush               = scratchFlush,
                                        fuseRelease             = scratchRelease,
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
             , statBlocks           = fromIntegral (fileSize status `div` 1024)
             , statAccessTime       = accessTime status
             , statModificationTime = modificationTime status
             , statStatusChangeTime = statusChangeTime status
             }

(<//>):: FilePath -> FilePath -> FilePath
(<//>) a b = normalise (a ++ "/" ++ b)

scratchGetFileStat:: FilePath -> FilePath -> IO (Either Errno FileStat)
scratchGetFileStat r s = do
    exists <- fileExist path
    if exists
      then do stat <- getSymbolicLinkStatus path 
              return $ Right $ fileStatusToFileStat stat
      else return $ Left eOK
    where
      path = r <//> s



scratchCreateDirectory:: FilePath -> FilePath -> FileMode -> IO Errno
scratchCreateDirectory r p m = createDirectory (r <//> p) m >> return eOK

scratchRemoveDirectory:: FilePath -> FilePath -> IO Errno
scratchRemoveDirectory r p = removeDirectory (r <//> p) >> return eOK

scratchOpenDirectory:: FilePath -> FilePath -> IO Errno
scratchOpenDirectory r p = openDirStream (r <//> p) >>= closeDirStream >> return eOK

scratchReadDirectory :: FilePath -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
scratchReadDirectory r p = getDirectoryContents (r <//> p) >>= mapM pairType >>= return . Right
    where pairType name = do
              status <- getSymbolicLinkStatus (r <//> name)
              return (name, fileStatusToFileStat status)

scratchRename :: FilePath -> FilePath -> IO Errno
scratchRename src dest = syslog Debug ("Rename: " ++ src ++ " -> " ++ dest) >> rename src dest >> return eOK

scratchSetFileMode :: FilePath -> FileMode -> IO Errno
scratchSetFileMode path mode = setFileMode path mode >> return eOK

scratchSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno
scratchSetOwnerAndGroup path uid gid = setOwnerAndGroup path uid gid  >> return eOK

scratchSetFileSize :: FilePath -> FileOffset -> IO Errno
scratchSetFileSize path off = setFileSize path off >> return eOK

scratchSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno
scratchSetFileTimes path aTime mTime = setFileTimes path aTime mTime >> return eOK

scratchOpen :: FilePath -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Fd)
scratchOpen r p mode flags = openFd (r <//> p) mode (Just stdFileMode) flags >>= return.Right

scratchRead :: FilePath -> FilePath -> Fd -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
scratchRead r p fd count off = do
    newOff <- fdSeek fd AbsoluteSeek off
    if off /= newOff
        then return (Left eINVAL)
        else do (content, bytesRead) <- fdRead fd count
                return (Right $ B.pack content)

scratchWrite :: FilePath -> IORef State -> FilePath -> Fd -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
scratchWrite r st p fd buf off = do
    newOff <- fdSeek fd AbsoluteSeek off
    if off /= newOff
        then return (Left eINVAL)
        else do
            w <- fdWrite fd (B.unpack buf) 
            modifyIORef st (\st' -> st' {size = (size st' + fromIntegral w)})
            nSt <- readIORef st 
            syslog Debug ("New size: " ++ show (size nSt))
            return $ Right w

scratchGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
scratchGetFileSystemStats _ = return (Left eOK)

scratchFlush :: FilePath -> Fd -> IO Errno
scratchFlush _ _ = return eOK

scratchRelease :: FilePath -> Fd -> IO ()
scratchRelease _ = closeFd

scratchSynchronizeFile :: FilePath -> SyncType -> IO Errno
scratchSynchronizeFile _ _ = return eOK
