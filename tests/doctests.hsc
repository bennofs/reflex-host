{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides doctests for a project based on the actual versions
-- of the packages it was built with. It requires a corresponding Setup.lhs
-- to be added to the project
-----------------------------------------------------------------------------
module Main where

import Build_doctests (doctestTargets)
import Control.Monad
import Test.DocTest

##ifdef mingw32_HOST_ARCH
##ifdef i386_HOST_ARCH
##define USE_CP
import Control.Applicative
import Control.Exception
import Foreign.C.Types
foreign import stdcall "windows.h SetConsoleCP" c_SetConsoleCP :: CUInt -> IO Bool
foreign import stdcall "windows.h GetConsoleCP" c_GetConsoleCP :: IO CUInt
##elif defined(x86_64_HOST_ARCH)
##define USE_CP
import Control.Applicative
import Control.Exception
import Foreign.C.Types
foreign import ccall "windows.h SetConsoleCP" c_SetConsoleCP :: CUInt -> IO Bool
foreign import ccall "windows.h GetConsoleCP" c_GetConsoleCP :: IO CUInt
##endif
##endif

-- | Run in a modified codepage where we can print UTF-8 values on Windows.
withUnicode :: IO a -> IO a
##ifdef USE_CP
withUnicode m = do
  cp <- c_GetConsoleCP
  (c_SetConsoleCP 65001 >> m) `finally` c_SetConsoleCP cp
##else
withUnicode m = m
##endif

main :: IO ()
main = withUnicode $ do
  forM_ doctestTargets $ \(name, mods, dirs, dep) -> do
    putStrLn $ ":: Running doctests for " ++ name
    doctest $
        "-idist/build/autogen"
      : "-idist/build"
      : "-optP-include"
      : "-optPdist/build/autogen/cabal_macros.h"
      : "-package-dbdist/package.conf.inplace"
      : "-hide-all-packages"
      : map ("-package="++) dep
        ++ map ("-i"++) dirs
        ++ mods
