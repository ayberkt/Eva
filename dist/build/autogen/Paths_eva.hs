module Paths_eva (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/ayberkt/Developer/eva/.cabal-sandbox/bin"
libdir     = "/Users/ayberkt/Developer/eva/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.3/eva-0.1.0.0"
datadir    = "/Users/ayberkt/Developer/eva/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/eva-0.1.0.0"
libexecdir = "/Users/ayberkt/Developer/eva/.cabal-sandbox/libexec"
sysconfdir = "/Users/ayberkt/Developer/eva/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "eva_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "eva_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "eva_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "eva_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "eva_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
