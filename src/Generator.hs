module Main where

import System.Environment (getArgs)
import System.Random (StdGen, newStdGen, split, uniformR)
import System.Random.Shuffle (shuffle')

-- Recursive perumtation generator
generateInput :: [[Int]] -> StdGen -> Int -> Int -> [[String]]
generateInput acc gen n 0 = map (map show) acc -- Base case, convert all the Ints to Strings
generateInput acc gen n k =
  generateInput
    (shuffle' [1 .. n] n gen : acc) -- Create a random permutation and append it to the accumulator
    (fst (split gen)) -- Split the generator. ^1
    n
    (k -1)

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

randInts :: [Int] -> StdGen -> Int -> Int -> Int -> [Int]
randInts acc gen min max 0 = acc
randInts acc gen min max count = do
  let (value, newGen) = uniformR (min, max) gen
  randInts (value : acc) newGen min max (count -1)

main :: IO ()
main = do
  args <- getArgs -- Command line arguments
  gen <- newStdGen -- New random number generator
  let (gen1, gen2) = split gen -- There's probably a better way to handle this
  let (gen3, gen4) = split gen1
  let gen5 = split gen2
  let n = read (head args) :: Int -- `head [any]` returns the first item in a list
  let k = read (args !! 1) :: Int -- `[any] !! Integer` returns the nth item in the list
  print n
  print k
  putChar '\n'
  printLists (generateInput [] gen1 n k) " " -- Preference lists for hospitals
  putChar '\n'
  printLists (generateInput [] gen2 n k) " " -- Preference lists for students
  putChar '\n'
  printList (map show (randInts [] gen3 1 k n)) " " -- Hospital to preference list mapping
  putChar '\n'
  printList (map show (randInts [] gen4 1 k n)) " " -- Student to preference list mapping

-- 1: Since Haskell is purely functional, it does not allow side-effects (except inside monads,
--    but I haven't gotten that far yet). Thus every time gen is used, it gives the same output.
--    To get unique values, we split the generator before passing it into the next level of recursion.
--    Split takes a generator and returns two new distinct generators (I only use one of them here).
