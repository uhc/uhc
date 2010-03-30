-- [###] Added Haskel98 Direcotory module. This can be copied with the help of the configuration file.
module Directory (
    Permissions( Permissions, readable, writable, executable, searchable ), 
    createDirectory, removeDirectory,{- removeFile, 
    renameDirectory, renameFile,-} getDirectoryContents,
    getCurrentDirectory, setCurrentDirectory,
    doesFileExist, doesDirectoryExist,
    getPermissions, setPermissions,
    getModificationTime 
  ) where

import System.Directory
