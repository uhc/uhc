{- ----------------------------------------------------------------------------------------
   what    : Working with directories
   expected: ok
   platform: environment dependent output
---------------------------------------------------------------------------------------- -}

module Main where

import System.Directory
import Data.Maybe

pFile :: String
pFile = "filesForIOTesting/empty"

main :: IO ()
main = do
  findExecutable "gcc" >>= (print . isJust)

  getHomeDirectory >>= print
  getAppUserDataDirectory "gcc" >>= print
  getUserDocumentsDirectory >>= print
  getTemporaryDirectory >>= print

  getModificationTime pFile >>= print


