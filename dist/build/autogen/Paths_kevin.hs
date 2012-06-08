module Paths_kevin (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/jdt/bin"
libdir     = "/Users/jdt/lib/kevin-0.0/ghc-7.4.1"
datadir    = "/Users/jdt/share/kevin-0.0"
libexecdir = "/Users/jdt/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "kevin_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "kevin_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "kevin_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "kevin_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
