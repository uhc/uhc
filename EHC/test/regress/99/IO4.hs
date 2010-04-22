{- ----------------------------------------------------------------------------------------
   what    : System.IO
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

import System.IO
import System.Directory

file :: FilePath
file = "filesForIOTesting/file1"

newFile :: FilePath
newFile = "filesForIOTesting/newFile"

main :: IO ()
main
  do c <- readFile file
     putStrlLn c
     writeFile  c newFile
     appendFile c newFile
     c' <- readFile newFile
     print (c' = c ++ c)
     removeFile newFile

     h <- openFile file ReadMode
     hFileSize h   >>= print
     hIsOpen h     >>= print
     hIsClosed h   >>= print
     hIsReadable h >>= print
     hIsWritable h >>= print
     hIsSeekable h >>= print
     
     hClose h
     
     hIsOpen h     >>= print
     hIsClosed h   >>= print
     hIsReadable h >>= print
     hIsWritable h >>= print
     hIsSeekable h >>= print

