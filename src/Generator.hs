module Main where

import System.Environment (getArgs)
import System.Random (StdGen, newStdGen, split, uniformR)
import System.Random.Shuffle (shuffle')

-- Recursive perumtation generator
doGenerateInput :: [[Int]] -> StdGen -> Int -> Int -> [[String]]
doGenerateInput acc gen n 0 = map (map show) acc -- Base case, convert all the Ints to Strings
doGenerateInput acc gen n k =
  doGenerateInput
    (shuffle' [1 .. n] n gen : acc) -- Create a random permutation and append it to the accumulator
    (fst (split gen)) -- Split the generator. ^1
    n
    (k -1)

generateInput :: Int -> Int -> IO [[String]]
generateInput n k = do
  gen <- newStdGen
  return (doGenerateInput [] gen n k)

-- Takes a list of strings and prints each element separated by some separator
printList :: [String] -> String -> IO ()
printList [] separator = putChar '\n'
printList [x] separator = putStr (x ++ "\n")
printList (x : xs) separator = do
  putStr x
  putStr separator
  printList xs separator

printLists :: [[String]] -> String -> IO ()
printLists [] separator = return ()
printLists (x : xs) separator = do
  printList x separator
  printLists xs separator

doRandInts :: [Int] -> StdGen -> Int -> Int -> Int -> [Int]
doRandInts acc gen min max 0 = acc
doRandInts acc gen min max count = do
  let (value, newGen) = uniformR (min, max) gen
  doRandInts (value : acc) newGen min max (count -1)

randInts :: Int -> Int -> Int -> IO [Int]
randInts min max count = do
  gen <- newStdGen
  return (doRandInts [] gen min max count)

main :: IO ()
main = do
  args <- getArgs -- Command line arguments
  let n = read (head args) :: Int -- `head [any]` returns the first item in a list
  let k = read (args !! 1) :: Int -- `[any] !! Integer` returns the nth item in the list
  hospitalPreferenceLists <- generateInput n k
  studentPreferenceLists <- generateInput n k
  hospitalMap <- randInts 1 k n
  studentMap <- randInts 1 k n
  print n
  print k
  putChar '\n'
  printLists hospitalPreferenceLists " " -- Preference lists for hospitals
  putChar '\n'
  printLists studentPreferenceLists " " -- Preference lists for students
  putChar '\n'
  printList (map show hospitalMap) " " -- Hospital to preference list mapping
  putChar '\n'
  printList (map show studentMap) " " -- Student to preference list mapping

-- 1: Since Haskell is purely functional, it does not allow side-effects (except inside monads,
--    but I haven't gotten that far yet). Thus every time gen is used, it gives the same output.
--    To get unique values, we split the generator before passing it into the next level of recursion.
--    Split takes a generator and returns two new distinct generators (I only use one of them here).
