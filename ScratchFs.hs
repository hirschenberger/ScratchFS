module Main where

import           Control.Exception
import           Control.Monad
import           System.Directory    (getDirectoryContents)
import           System.Fuse
import           System.IO
import           System.Posix
import           System.Posix.Syslog
import           System.Environment
import           System.FilePath.Posix ((</>))
import qualified Data.ByteString.Char8 as B



main:: IO ()
main = withSyslog "ScratchFS" [PID, PERROR] USER $ do
          let rootDir = "/tmp/mnt"
          syslog Debug ("Starting ScratchFS on" ++ show rootDir)
          fuseMain (scratchOps rootDir) exceptionHandler

exceptionHandler:: SomeException -> IO Errno
exceptionHandler e = syslog Error ("Exception: " ++ show e) >> defaultExceptionHandler e

scratchOps:: FilePath -> FuseOperations Fd
scratchOps root = defaultFuseOps {  fuseGetFileStat         = scratchGetFileStat root,
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
                                    fuseRelease             = scratchRelease,
                                    fuseSynchronizeFile     = scratchSynchronizeFile
                                  }


fileStatusToEntryType :: FileStatus -> EntryType
fileStatusToEntryType status
    | isSymbolicLink status = SymbolicLink
    | isNamedPipe status = NamedPipe
    | isCharacterDevice status = CharacterSpecial
    | isDirectory status = Directory
    | isBlockDevice status = BlockSpecial
    | isRegularFile status = RegularFile
    | isSocket status = Socket
    | otherwise = Unknown

fileStatusToFileStat :: FileStatus -> FileStat
fileStatusToFileStat status =
    FileStat { statEntryType = fileStatusToEntryType status
             , statFileMode = fileMode status
             , statLinkCount = linkCount status
             , statFileOwner = fileOwner status
             , statFileGroup = fileGroup status
             , statSpecialDeviceID = specialDeviceID status
             , statFileSize = fileSize status
             -- fixme: 1024 is not always the size of a block
             , statBlocks = fromIntegral (fileSize status `div` 1024)
             , statAccessTime = accessTime status
             , statModificationTime = modificationTime status
             , statStatusChangeTime = statusChangeTime status
             }

scratchGetFileStat:: FilePath -> FilePath -> IO (Either Errno FileStat)
scratchGetFileStat r s = do
    stat <- getFileStatus (r </> s)
    return $ Right $ fileStatusToFileStat stat


scratchCreateDirectory:: FilePath -> FilePath -> FileMode -> IO Errno
scratchCreateDirectory r p m = createDirectory (r </> p) m >> return eOK

scratchRemoveDirectory:: FilePath -> FilePath -> IO Errno
scratchRemoveDirectory r p = removeDirectory (r </> p) >> return eOK

scratchOpenDirectory:: FilePath -> FilePath -> IO Errno
scratchOpenDirectory r p = openDirStream (r </> p) >>= closeDirStream >> return eOK

scratchReadDirectory :: FilePath -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
scratchReadDirectory r p = getDirectoryContents (r </> p) >>= mapM pairType >>= return . Right
    where pairType name = do
              status <- getSymbolicLinkStatus ((r </> p) ++ "/" ++ name)
              return (name, fileStatusToFileStat status)

scratchRename :: FilePath -> FilePath -> IO Errno
scratchRename src dest = rename src dest >> return eOK

scratchSetFileMode :: FilePath -> FileMode -> IO Errno
scratchSetFileMode path mode = setFileMode path mode >> return eOK

scratchSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno
scratchSetOwnerAndGroup path uid gid = setOwnerAndGroup path uid gid  >> return eOK

scratchSetFileSize :: FilePath -> FileOffset -> IO Errno
scratchSetFileSize path off = setFileSize path off >> return eOK

scratchSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno
scratchSetFileTimes path aTime mTime = setFileTimes path aTime mTime >> return eOK

scratchOpen :: FilePath -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Fd)
scratchOpen root path mode flags = openFd (root </> path) mode Nothing flags >>= return.Right

scratchRead :: FilePath -> FilePath -> Fd -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
scratchRead root path fd count off = do
    newOff <- fdSeek fd AbsoluteSeek off
    if off /= newOff
        then return (Left eINVAL)
        else do (content, bytesRead) <- fdRead fd count
                return (Right $ B.pack content)

scratchWrite :: FilePath -> FilePath -> Fd -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
scratchWrite root path fd buf off = do
    newOff <- fdSeek fd AbsoluteSeek off
    if off /= newOff
        then return (Left eINVAL)
        else fdWrite fd (B.unpack buf) >>= return.Right

scratchGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
scratchGetFileSystemStats _ = return (Left eOK)

scratchFlush :: FilePath -> Fd -> IO Errno
scratchFlush _ _ = return eOK

scratchRelease :: FilePath -> Fd -> IO ()
scratchRelease _ = closeFd 

scratchSynchronizeFile :: FilePath -> SyncType -> IO Errno
scratchSynchronizeFile _ _ = return eOK
