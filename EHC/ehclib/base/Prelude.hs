{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Prelude
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Prelude: a standard module imported by default into all Haskell
-- modules.  For more documentation, see the Haskell 98 Report
-- <http://www.haskell.org/onlinereport/>.
--
-----------------------------------------------------------------------------

module Prelude
  ( module UHC.Base
  , module UHC.Eq
  , module UHC.Ord
  , module UHC.Enum
  , module UHC.Bounded
  , module UHC.Ix
  , module UHC.Show
  , module UHC.Read
  , module UHC.Run
--  , module UHC.Handle
  , module System.IO
  , module UHC.IOBase
  )
  where

import UHC.Base hiding
  ( absReal, signumReal
  -- , primEqInt
  -- , State, IOWorld, RealWorld
  , ByteArray
  , exitWithIntCode
  )
import UHC.Eq
import UHC.Ord
import UHC.Enum
import UHC.Bounded
import UHC.Ix
import UHC.Show
import UHC.Read
-- [###] Only this function are actualy exported by GHC prelude. Handle is not exported in the GHC Predude; is uhc design to export it?
import UHC.IOBase ( IOError, ioError, userError, catch, unsafePerformIO )
-- import UHC.Handle -- hiding ( hPutStrLn )
import UHC.Run

import UHC.TestX -- [@@@] for testing purpose

-- [###] The IO function exported by  GHC prelude are from System.IO
-- [@@@] Check what prelude export
import System.IO
{-  ( IO,
    -- *** Output functions
    putChar,
    putStr, putStrLn, print,
    -- *** Input functions
    getChar,
    getLine, getContents, interact,
    -- *** Files
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn,
    openFile
  )
-}
