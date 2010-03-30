module UHC.TestX  where -- [###] [BUG] cannot export operators?

--import Data.Maybe
import UHC.Base
--import UHC.IO
--import UHC.IOBase
--import UHC.Handle

testdummy :: Int -> Int
testdummy x = x

foreign import ccall unsafe "HsBase.h __hscore_supportsTextMode"
  tEXT_MODE_SEEK_ALLOWED :: Bool


