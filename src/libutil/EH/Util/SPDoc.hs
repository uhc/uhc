-------------------------------------------------------------------------
-- Simple Printing Doc: minimalistic pretty printing.
--
-- Features/limitations:
--   * implistic susport for indentation, not for verticals inside horizontal,
--     the resulting inaccuracy should not matter to the user.
-------------------------------------------------------------------------

module EH.Util.SPDoc
  ( SPDoc, empty
  , SP(..)
  
  , (>|<), (>-<), (>#<)
  
  , spHList, spVList
  , spBlock
  , spCurlysBlock, spCurlysSemisBlock, spCurlysCommasBlock
  , spParensSemisBlock, spParensCommasBlock
  
  , hSPPut
  )
  where

import IO
import qualified UU.Pretty as P

-------------------------------------------------------------------------
-- Doc structure
-------------------------------------------------------------------------

data SPDoc
  = Emp
  | Str			!String					-- basic string
  | Hor			!SPDoc !SPDoc			-- horizontal positioning
  | Ver			!SPDoc !SPDoc			-- vertical positioning
  | Ind			!Int !SPDoc				-- indent
  deriving Show

empty :: SPDoc
empty = Emp

-------------------------------------------------------------------------
-- Class interface + basic instances
-------------------------------------------------------------------------

class SP x where
  sp :: x -> SPDoc

instance SP SPDoc where
  sp = id

instance SP String where
  sp = Str

instance SP P.PP_Doc where
  sp x = sp $ P.disp x 2000 ""

instance SP Int where
  sp = sp . show

-------------------------------------------------------------------------
-- Basic construction
-------------------------------------------------------------------------

infixr 3 >|<, >#<
infixr 2 >-<

(>|<) :: (SP a, SP b) => a -> b -> SPDoc
a >|< b = sp a `Hor` sp b

(>#<) :: (SP a, SP b) => a -> b -> SPDoc
a >#< b = a >|< " " >|< b

(>-<) :: (SP a, SP b) => a -> b -> SPDoc
a >-< b = sp a `Ver` sp b

-------------------------------------------------------------------------
-- Derived construction
-------------------------------------------------------------------------

spHList :: SP a => [a] -> SPDoc
spHList = foldr (>|<) empty

spVList :: SP a => [a] -> SPDoc
spVList = foldr (>-<) empty

spBlock :: SP a => String -> String -> String -> [a] -> SPDoc
spBlock o c s []     = o >|< c
spBlock o c s [a]    = o >|< a >|< c
spBlock o c s (a:as) = o >|< a >-< (spVList $ map (s >|<) as) >-< c

spCurlysBlock :: SP a => [a] -> SPDoc
spCurlysBlock = spBlock "{ " "}" "  " . map sp

spCurlysSemisBlock :: SP a => [a] -> SPDoc
spCurlysSemisBlock = spBlock "{ " "}" "; " . map sp

spCurlysCommasBlock :: SP a => [a] -> SPDoc
spCurlysCommasBlock = spBlock "{ " "}" ", " . map sp

spParensSemisBlock :: SP a => [a] -> SPDoc
spParensSemisBlock = spBlock "( " ")" "; " . map sp

spParensCommasBlock :: SP a => [a] -> SPDoc
spParensCommasBlock = spBlock "( " ")" ", " . map sp

-------------------------------------------------------------------------
-- Semantics: direct IO
-------------------------------------------------------------------------

hSPPut :: SPDoc -> Handle -> IO ()
hSPPut d h = hSPPut' d "" h

hSPPut' :: SPDoc -> String -> Handle -> IO ()
hSPPut' d ind h
  = case d of
      Emp              -> return ()
      Str s            -> hPutStr h s
      Ind i  d         -> hSPPut' d (ind ++ replicate i ' ') h
      Hor d1 d2        -> do hSPPut' d1 ind h
                             hSPPut' d2 ind h
      Ver Emp d2       -> hSPPut' d2 ind h
      Ver d1 Emp       -> hSPPut' d1 ind h
      Ver d1 d2        -> do hSPPut' d1 ind h
                             hPutStrLn h ind
                             hSPPut' d2 ind h

-------------------------------------------------------------------------
-- Semantics: PP
-------------------------------------------------------------------------

instance P.PP SPDoc where
  pp Emp       = P.empty
  pp (Str x  ) = P.pp x
  pp (Hor x y) = x P.>|< y
  pp (Ver x y) = x P.>-< y
  pp (Ind i x) = P.indent i (P.pp x)

