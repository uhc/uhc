module EH.Util.FPath 
  ( FPath(..), fpathSuff
  , FPATH(..)
  , emptyFPath
  -- , mkFPath
  , mkFPathFromDirsFile
  , fpathToStr, fpathIsEmpty
  , fpathSetBase, fpathSetSuff, fpathSetDir
  , fpathUpdBase
  , fpathRemoveSuff, fpathRemoveDir
  , fpathPrependDir, mkTopLevelFPath
  
  , fpathDirSep, fpathDirSepChar
  
  , fpathOpenOrStdin, openFPath
  
  , SearchPath, FileSuffixes
  , mkInitSearchPath, searchPathFromFPath, searchPathFromFPaths
  , searchPathFromString
  , searchLocationsForReadableFiles, searchPathForReadableFiles, searchPathForReadableFile
  
  , fpathEnsureExists
  )
where

import IO
import Data.Maybe
import Data.List
import Control.Monad
import System.IO
import System.Directory

-------------------------------------------------------------------------------------------
-- File path utils
-------------------------------------------------------------------------------------------

data FPath
  = FPath
      { fpathMbDir      :: !(Maybe  String)
      , fpathBase       ::         !String
      , fpathMbSuff     :: !(Maybe  String)
      }
    deriving (Show,Eq,Ord)

emptyFPath :: FPath
emptyFPath
  = mkFPath ""

fpathIsEmpty :: FPath -> Bool
fpathIsEmpty fp = null (fpathBase fp)

fpathToStr :: FPath -> String
fpathToStr fpath
  = let adds f = maybe f (\s -> f ++ "."         ++ s) (fpathMbSuff fpath)
        addd f = maybe f (\d -> d ++ fpathDirSep ++ f) (fpathMbDir fpath)
     in addd . adds . fpathBase $ fpath

fpathFromStr :: String -> FPath
fpathFromStr fn
  = FPath d b' s
  where (d ,b) = maybe (Nothing,fn) (\(d,b) -> (Just d,b)) (splitOnLast fpathDirSepChar fn)
        (b',s) = maybe (b,Nothing)  (\(b,s) -> (b,Just s)) (splitOnLast '.'             b )

fpathSuff :: FPath -> String
fpathSuff = maybe "" id . fpathMbSuff

fpathSetBase :: String -> FPath -> FPath
fpathSetBase s fp
  = fp {fpathBase = s}

fpathUpdBase :: (String -> String) -> FPath -> FPath
fpathUpdBase u fp
  = fp {fpathBase = u (fpathBase fp)}

fpathSetSuff :: String -> FPath -> FPath
fpathSetSuff "" fp
  = fpathRemoveSuff fp
fpathSetSuff s fp
  = fp {fpathMbSuff = Just s}

fpathSetNonEmptySuff :: String -> FPath -> FPath
fpathSetNonEmptySuff "" fp
  = fp
fpathSetNonEmptySuff s fp
  = fp {fpathMbSuff = Just s}

fpathSetDir :: String -> FPath -> FPath
fpathSetDir "" fp
  = fpathRemoveDir fp
fpathSetDir d fp
  = fp {fpathMbDir = Just d}

fpathPrependDir :: String -> FPath -> FPath
fpathPrependDir "" fp
  = fp
fpathPrependDir d fp
  = maybe (fpathSetDir d fp) (\fd -> fpathSetDir (d ++ fpathDirSep ++ fd) fp) (fpathMbDir fp)

fpathRemoveSuff :: FPath -> FPath
fpathRemoveSuff fp
  = fp {fpathMbSuff = Nothing}

fpathRemoveDir :: FPath -> FPath
fpathRemoveDir fp
  = fp {fpathMbDir = Nothing}

splitOnLast :: Char -> String -> Maybe (String,String)
splitOnLast splitch fn
  = case fn of
      ""     -> Nothing
      (f:fs) -> let rem = splitOnLast splitch fs
                 in if f == splitch
                    then maybe (Just ("",fs)) (\(p,s)->Just (f:p,s)) rem
                    else maybe Nothing (\(p,s)->Just (f:p,s)) rem

{-
mkFPath :: String -> FPath
mkFPath fn
  = let (d,b)  = maybe (Nothing,fn) (\(d,b) -> (Just d,b)) (splitOnLast fpathDirSepChar fn)
        (b',s) = maybe (b,Nothing) (\(b,s) -> (b,Just s)) (splitOnLast '.' b)
     in FPath d b' s
-}

mkFPathFromDirsFile :: Show s => [s] -> s -> FPath
mkFPathFromDirsFile dirs f
  = fpathSetDir (concat $ intersperse fpathDirSep $ map show $ dirs) (mkFPath (show f))

fpathSplit :: String -> (String,String)
fpathSplit fn
  = let (d,b)  = maybe ("",fn) id (splitOnLast fpathDirSepChar fn)
        (b',s) = maybe (b,"") id (splitOnLast '.' b)
        b''    = if null d then b' else d ++ fpathDirSep ++ b'
     in (b'',s)

mkTopLevelFPath :: String -> String -> FPath
mkTopLevelFPath suff fn
  = let fpNoSuff = mkFPath fn
     in maybe (fpathSetSuff suff fpNoSuff) (const fpNoSuff) . fpathMbSuff $ fpNoSuff

-------------------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------------------

fpathDirSep :: String
fpathDirSep = "/"

fpathDirSepChar :: Char
fpathDirSepChar = head fpathDirSep

-------------------------------------------------------------------------------------------
-- Class 'can make FPath of ...'
-------------------------------------------------------------------------------------------

class FPATH f where
  mkFPath :: f -> FPath

instance FPATH String where
  mkFPath = fpathFromStr

instance FPATH FPath where
  mkFPath = id

-------------------------------------------------------------------------------------------
-- Open path for read or return stdin
-------------------------------------------------------------------------------------------

fpathOpenOrStdin :: FPath -> IO (FPath,Handle)
fpathOpenOrStdin fp
  = if fpathIsEmpty fp
    then return (mkFPath "<stdin>",stdin)
    else do { let fn = fpathToStr fp
            ; h <- openFile fn ReadMode
            ; return (fp,h)
            }

openFPath :: FPath -> IOMode -> Bool -> IO (String, Handle)
openFPath fp mode binary
  | fpathIsEmpty fp = case mode of
                        ReadMode      -> return ("<stdin>" ,stdin )
                        WriteMode     -> return ("<stdout>",stdout)
                        AppendMode    -> return ("<stdout>",stdout)
                        ReadWriteMode -> error "cannot use stdin/stdout with random access"
  | otherwise       = do { let fNm = fpathToStr fp
                         ; h <- if binary
                                then openBinaryFile fNm mode
                                else openFile fNm mode
                         ; return (fNm,h)
                         }

-------------------------------------------------------------------------------------------
-- Directory
-------------------------------------------------------------------------------------------

fpathEnsureExists :: FPath -> IO ()
fpathEnsureExists fp
  = do { let d = fpathMbDir fp
       ; when (isJust d) (createDirectoryIfMissing True (fromJust d))
       }

-------------------------------------------------------------------------------------------
-- Search path utils
-------------------------------------------------------------------------------------------

type SearchPath = [String]
type FileSuffixes = [String]

searchPathFromFPaths :: [FPath] -> SearchPath
searchPathFromFPaths fpL = nub [ d | (Just d) <- map fpathMbDir fpL ] ++ [""]

searchPathFromFPath :: FPath -> SearchPath
searchPathFromFPath fp = searchPathFromFPaths [fp]

mkInitSearchPath :: FPath -> SearchPath
mkInitSearchPath = searchPathFromFPath

searchPathFromString :: String -> [String]
searchPathFromString
  = unfoldr f
  where f "" = Nothing
        f sp = Just (break (== ';') sp)

searchLocationsForReadableFiles :: (loc -> String) -> Bool -> [loc] -> FileSuffixes -> FPath -> IO [(FPath,loc)]
searchLocationsForReadableFiles getdir stopAtFirst locs suffs fp
  = let select stop f fps
          = foldM chk [] fps
          where chk r fp
                  = case r of
                      (_:_) | stop -> return r
                      _            -> do r' <- f fp
                                         return (r ++ r')
        tryToOpen loc mbSuff fp
          = do { let fp' = maybe fp (\suff -> fpathSetNonEmptySuff suff fp) mbSuff
               ; fExists <- doesFileExist (fpathToStr fp')
               -- ; hPutStrLn stderr (show fp ++ " - " ++ show fp')
               ; if fExists
                 then return [(fp',loc)]
                 else return []
               }
        tryToOpenWithSuffs loc suffs fp
          = case suffs of
              [] -> tryToOpen loc Nothing fp
              _  -> select stopAtFirst
                      (\(ms,f) -> tryToOpen loc ms f)
                      ((Nothing,fp) : zipWith (\s f -> (Just s,f)) suffs (repeat fp))
        tryToOpenInDir loc
          = select True (tryToOpenWithSuffs loc suffs) [fpathPrependDir (getdir loc) fp {-,fpathSetDir dir fp -}]
     in select True tryToOpenInDir locs

searchPathForReadableFiles :: Bool -> SearchPath -> FileSuffixes -> FPath -> IO [FPath]
searchPathForReadableFiles stopAtFirst locs suffs fp
  = fmap (map fst) $ searchLocationsForReadableFiles id stopAtFirst locs suffs fp

searchPathForReadableFile :: SearchPath -> FileSuffixes -> FPath -> IO (Maybe FPath)
searchPathForReadableFile paths suffs fp
  = do fs <- searchPathForReadableFiles True paths suffs fp
       return (listToMaybe fs)

