module Main where

import Control.Monad (replicateM)
import Data.List.Split (splitWhen)
import Data.Map (Map, delete, empty, fromList, insert, toList, (!), (!?))
import Text.Printf (printf)

-- Takes a string of space separated values, converts it to a list of strings, and then
-- parses the strings to integers
readLine :: IO [Int]
readLine = map (\q -> read q :: Int) . splitWhen (== ' ') <$> getLine

-- Reads the input from stdin
readInput :: IO (Int, Int, [[Int]], [[Int]], [Int], [Int])
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

makeStudentValueMap :: Int -> [Int] -> Map Int Int
makeStudentValueMap n preferenceList = fromList (zip preferenceList [(n - 1), (n - 2) .. 0])

doGaleShapley ::
  [Int] ->
  Int ->
  Map Int Int ->
  Map Int Int ->
  Map Int [Int] ->
  Map Int (Map Int Int) ->
  (Map Int Int, Int)
doGaleShapley [] round hospitalsMap studentsMap _ _ = (hospitalsMap, round)
doGaleShapley unmatchedHospitals round hospitalsMap studentsMap hospitals students = do
  let hospitalId = head unmatchedHospitals
  let hospitalPreferences = hospitals ! hospitalId
  let hospitals' = insert hospitalId (tail hospitalPreferences) hospitals
  let studentId = head hospitalPreferences
  let studentPreferences = students ! studentId
  let maybeStudentCurrentHospitalId = studentsMap !? studentId
  case maybeStudentCurrentHospitalId of
    Nothing -> do
      doGaleShapley unmatchedHospitals' (round + 1) hospitalsMap' studentsMap' hospitals' students
      where
        unmatchedHospitals' = tail unmatchedHospitals
        hospitalsMap' = insert hospitalId studentId hospitalsMap
        studentsMap' = insert studentId hospitalId studentsMap
    Just studentCurrentHospitalId -> do
      doGaleShapley unmatchedHospitals' (round + 1) hospitalsMap' studentsMap' hospitals' students
      where
        (unmatchedHospitals', hospitalsMap', studentsMap') =
          if studentPreferences ! hospitalId > studentPreferences ! studentCurrentHospitalId
            then
              ( studentCurrentHospitalId : tail unmatchedHospitals,
                insert hospitalId studentId (delete studentCurrentHospitalId hospitalsMap),
                insert studentId hospitalId studentsMap
              )
            else
              ( hospitalId : tail unmatchedHospitals,
                hospitalsMap,
                studentsMap
              )

galeShapley ::
  Int ->
  Map Int [Int] ->
  Map Int (Map Int Int) ->
  (Map Int Int, Int)
galeShapley n = doGaleShapley unmatchedHospitals 0 hospitalsMap studentsMap
  where
    hospitalsMap = empty
    studentsMap = empty
    unmatchedHospitals = [0 .. (n - 1)]

main = do
  (n, k, h, s, hi, si) <- readInput
  let hospitals = fromList (map (\q -> (q, h !! (hi !! q))) [0 .. (n - 1)])
  let students = fromList (map (\q -> (q, makeStudentValueMap n (s !! (si !! q)))) [0 .. (n - 1)])
  let (mapping, rounds) = galeShapley n hospitals students
  let matchList = toList mapping
  print rounds
  mapM_ (uncurry (printf "%d %d\n")) matchList
