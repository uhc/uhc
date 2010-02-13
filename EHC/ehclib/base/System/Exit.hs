-----------------------------------------------------------------------------
-- |
-- Module      :  System.Exit
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Exiting the program.
--
-----------------------------------------------------------------------------

module System.Exit
    (
      ExitCode(ExitSuccess,ExitFailure)
    , exitWith      -- :: ExitCode -> IO a
    , exitFailure   -- :: IO a
    , exitSuccess   -- :: IO a
  ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
#endif

#ifdef __HUGS__
import Hugs.Prelude (ExitCode(..))
import Control.Exception.Base
#endif

#ifdef __NHC__
import System
  ( ExitCode(..)
  , exitWith
  )
#endif

#ifdef __UHC__
import UHC.IOBase
import UHC.OldException (throwIO)
#endif

-- ---------------------------------------------------------------------------
-- exitWith

-- | Computation 'exitWith' @code@ throws 'ExitException' @code@.
-- Normally this terminates the program, returning @code@ to the
-- program's caller.  Before the program terminates, any open or
-- semi-closed handles are first closed.
--
-- A program that fails in any other way is treated as if it had
-- called 'exitFailure'.
-- A program that terminates successfully without calling 'exitWith'
-- explicitly is treated as it it had called 'exitWith' 'ExitSuccess'.
--
-- As an 'ExitException' is not an 'IOError', 'exitWith' bypasses
-- the error handling in the 'IO' monad and cannot be intercepted by
-- 'catch' from the "Prelude".  However it is an 'Exception', and can
-- be caught using the functions of "Control.Exception".  This means
-- that cleanup computations added with 'Control.Exception.bracket'
-- (from "Control.Exception") are also executed properly on 'exitWith'.

#ifndef __NHC__
exitWith :: ExitCode -> IO a
#ifdef __UHC__
exitWith code = throwIO (ExitException code)
#else
exitWith ExitSuccess = throwIO ExitSuccess
exitWith code@(ExitFailure n)
  | n /= 0 = throwIO code
#ifdef __GLASGOW_HASKELL__
  | otherwise = ioError (IOError Nothing InvalidArgument "exitWith" "ExitFailure 0" Nothing)
#endif  /* __GLASGOW_HASKELL__ */
#endif  /* __UHC__ */
#endif  /* ! __NHC__ */

-- | The computation 'exitFailure' is equivalent to
-- 'exitWith' @(@'ExitFailure' /exitfail/@)@,
-- where /exitfail/ is implementation-dependent.
exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

-- | The computation 'exitSuccess' is equivalent to
-- 'exitWith' 'ExitSuccess', It terminates the program
-- sucessfully.
exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess

