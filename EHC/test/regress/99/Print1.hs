{- ----------------------------------------------------------------------------------------
   what    : hPrint, print
   expected: ok
---------------------------------------------------------------------------------------- -}

module Print1 where

main :: IO ()
main
  = do print (5::Int)
       print "aap"
       print True
       print 'a'

       
