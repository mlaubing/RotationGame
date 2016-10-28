{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Project (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\Uebung_Funktionale_Programmierung\\RotationGame\\Project\\.stack-work\\install\\089bf8d4\\bin"
libdir     = "D:\\Uebung_Funktionale_Programmierung\\RotationGame\\Project\\.stack-work\\install\\089bf8d4\\lib\\i386-windows-ghc-8.0.1\\Project-0.1.0.0-BQAUV2G50M66dry8GmjFk1"
datadir    = "D:\\Uebung_Funktionale_Programmierung\\RotationGame\\Project\\.stack-work\\install\\089bf8d4\\share\\i386-windows-ghc-8.0.1\\Project-0.1.0.0"
libexecdir = "D:\\Uebung_Funktionale_Programmierung\\RotationGame\\Project\\.stack-work\\install\\089bf8d4\\libexec"
sysconfdir = "D:\\Uebung_Funktionale_Programmierung\\RotationGame\\Project\\.stack-work\\install\\089bf8d4\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
