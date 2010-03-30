{- ----------------------------------------------------------------------------------------
   what    : Testing CPUTime
   expected: ok
   platform: run time depends on platform   
---------------------------------------------------------------------------------------- -}

module Main where

import System.CPUTime

main :: IO ()

main = do
  putStrLn $ show cpuTimePrecision
  time <- getCPUTime
  putStrLn $ show time
  putStrLn $ show $ f ([1..10000] :: [Int])
  time' <- getCPUTime
  putStrLn $ show time'

f :: [Int] -> Int
f xs = length xs + head xs
