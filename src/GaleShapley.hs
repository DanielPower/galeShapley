module Main where

import Control.Monad (replicateM)
import Data.List.Split (splitWhen)

-- Takes a string of space separated values, converts it to a list of strings, and then
-- parses the strings to integers
readLine :: IO [String]
readLine = splitWhen (== ' ') <$> getLine

-- Reads the input from stdin
readInput :: IO (Int, Int, [[String]], [[String]], [String], [String])
readInput = do
  n <- getLine
  k <- getLine
  let n' = read n :: Int -- Parse to int
  let k' = read k :: Int
  _ <- getLine -- Discard empty line
  h <- replicateM k' readLine -- Run readLine k' times, store the results as a list in h
  _ <- getLine
  s <- replicateM k' readLine
  _ <- getLine
  hi <- readLine
  _ <- getLine
  si <- readLine
  return (n', k', h, s, hi, si)

doGaleShapley :: [Int] -> [Int] -> Int -> Int -> [[String]] -> [[String]] -> [String] -> [String] -> ([String], [String])
doGaleShapley hx sx n k h s hi si = ([], [])

galeShapley :: Int -> Int -> [[String]] -> [[String]] -> [String] -> [String] -> ([String], [String])
galeShapley = doGaleShapley (replicate 5 0) (replicate 5 0)

main = do
  (n, k, h, s, hi, si) <- readInput
  print n
  print k
  print h
  print s
  print hi
  print si
  let (a, b) = galeShapley n k h s hi si
  print a
  print b
