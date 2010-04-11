{- ----------------------------------------------------------------------------------------
   what    : Working with directories
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

import System.Directory

root :: String
root = "filesForIOTesting"

pFile :: String
pFile = root ++ "/" ++ "empty"

pFile1 :: String
pFile1 = root ++ "/" ++ "empty1"

pFile2 :: String
pFile2 = root ++ "/" ++ "empty2"

main :: IO ()
main = do
  cd   <- getCurrentDirectory
  rcd  <- makeRelativeToCurrentDirectory cd
  putStrLn rcd
  setCurrentDirectory root
  cd'  <- getCurrentDirectory
  setCurrentDirectory cd
  rcd' <- makeRelativeToCurrentDirectory cd'
  print (root == rcd')

  croot <- canonicalizePath root
  crcd' <- canonicalizePath cd'
  putStrLn $ show (croot == crcd')
  
  getDirectoryContents root >>= print
  

  perm   <- getPermissions pFile
  print perm
  setPermissions pFile perm{executable=True}
  perm'  <- getPermissions pFile
  print perm'
  setPermissions pFile perm'{executable=False}
  perm'' <- getPermissions pFile
  print (perm == perm'')

  doesFileExist pFile >>= print

  copyFile pFile pFile1
  doesFileExist pFile  >>= print
  doesFileExist pFile1  >>= print
  renameFile pFile1 pFile2
  doesFileExist pFile1  >>= print
  doesFileExist pFile2  >>= print
  removeFile pFile2
  doesFileExist pFile2  >>= print
  
