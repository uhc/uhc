{- ----------------------------------------------------------------------------------------
   what    : weak ptr implicit finalization
   expected: finalization messages
   note    : output depends on inlining. Currently for bc backend "fin1" should appear
---------------------------------------------------------------------------------------- -}

module WeakPtr2 where

import UHC.WeakPtr
import UHC.Weak
import UHC.GC
import Debug.Trace

f1 :: Int -> IO ()
f1 _ = do let x = [23] :: [Int]
          print x
          w <- mkWeak x x (Just $ print "fin1")
          x2 <- trace "f1" deRefWeak w
          trace "p1" print x2
          

f2 :: Int -> IO ()
f2 _ = do let x = [45] :: [Int]
          print x
          w <- mkWeak x x (Just $ print "fin2")
          x2 <- deRefWeak w
          f1 5
          trace "p2" print x2

main = do f2 3
          trace "fin" gc
          trace "ret" return ()

