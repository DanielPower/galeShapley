module Main where

import Control.Monad (replicateM)
import Data.List.Split (splitWhen)

-- Takes a string of space separated values, converts it to a list of strings, and then
-- parses the strings to integers
readLine :: IO [Int]
readLine =
  map
    (\p -> read p :: Int)
    . splitWhen (== ' ')
    <$> getLine

-- Reads the input from stdin
readInput :: IO (Int, Int, [[Int]], [[Int]], [Int], [Int])
readInput = do
  _n <- getLine
  _k <- getLine
  let n = read _n :: Int
  let k = read _k :: Int
  _ <- getLine -- Discard empty line
  h <- replicateM k readLine
  _ <- getLine
  s <- replicateM k readLine
  _ <- getLine
  hi <- readLine
  _ <- getLine
  si <- readLine
  return (n, k, h, s, hi, si)

doGaleShapley :: [Int] -> [Int] -> Int -> Int -> [[Int]] -> [[Int]] -> [Int] -> [Int] -> IO ()
doGaleShapley hx sx n k h s hi si =
  print "foo"

galeShapley :: Int -> Int -> [[Int]] -> [[Int]] -> [Int] -> [Int] -> IO ()
galeShapley = doGaleShapley (replicate 5 0) (replicate 5 0)

main = do
  (n, k, h, s, hi, si) <- readInput
  print n
  print k
  print h
  print s
  print hi
  print si
