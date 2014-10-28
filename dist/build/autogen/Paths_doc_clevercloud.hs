module Paths_doc_clevercloud (
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

bindir     = "/Users/clement/work/clever-cloud/clever_intern_work/hakyll-sandbox/bin"
libdir     = "/Users/clement/work/clever-cloud/clever_intern_work/hakyll-sandbox/lib/x86_64-osx-ghc-7.8.3/doc-clevercloud-0.1.0.0"
datadir    = "/Users/clement/work/clever-cloud/clever_intern_work/hakyll-sandbox/share/x86_64-osx-ghc-7.8.3/doc-clevercloud-0.1.0.0"
libexecdir = "/Users/clement/work/clever-cloud/clever_intern_work/hakyll-sandbox/libexec"
sysconfdir = "/Users/clement/work/clever-cloud/clever_intern_work/hakyll-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "doc_clevercloud_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "doc_clevercloud_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "doc_clevercloud_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "doc_clevercloud_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "doc_clevercloud_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
