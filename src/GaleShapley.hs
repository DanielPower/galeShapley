module Main where

import Control.Monad (replicateM)
import Data.List.Split (splitWhen)
import Data.Map (Map, delete, empty, fromList, insert, toList, (!), (!?))
import Text.Printf (printf)

-- Takes a string of space separated values, converts it to a list of strings, and then
-- parses the strings to integers
readLine :: IO [Int]
readLine = map (\q -> read q :: Int) . splitWhen (== ' ') <$> getLine

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

-- To the best of knowledge this runs in O(n) with respect to the size of the input, as long as you
-- assume hash map operations are O(1).
-- Footnote !1
doGaleShapley :: [Int] -> Int -> Map Int Int -> Map Int Int -> Map Int [Int] -> Map Int (Map Int Int) -> (Map Int Int, Int)
doGaleShapley [] round hospitalsMap studentsMap _ _ = (hospitalsMap, round) -- Base case returns the accumulators
doGaleShapley unmatchedHospitals round hospitalsMap studentsMap hospitals students =
  doGaleShapley unmatchedHospitals' (round + 1) hospitalsMap' studentsMap' hospitals' students
  where
    hospitalId = head unmatchedHospitals
    hospitalPreferences = hospitals ! hospitalId -- (!) is the get operator for Map
    hospitals' = insert hospitalId (tail hospitalPreferences) hospitals
    studentId = head hospitalPreferences
    studentPreferences = students ! studentId
    maybePreviousHospitalId = studentsMap !? studentId -- Footnote !2
    (unmatchedHospitals', hospitalsMap', studentsMap') = case maybePreviousHospitalId of
      Nothing ->
        ( tail unmatchedHospitals,
          insert hospitalId studentId hospitalsMap,
          insert studentId hospitalId studentsMap
        )
      Just previousHospitalId ->
        if studentPreferences ! hospitalId > studentPreferences ! previousHospitalId
          then
            ( previousHospitalId : tail unmatchedHospitals,
              insert hospitalId studentId (delete previousHospitalId hospitalsMap),
              insert studentId hospitalId studentsMap
            )
          else
            ( hospitalId : tail unmatchedHospitals,
              hospitalsMap,
              studentsMap
            )

-- Wrapper to simplify external usage of the galeShapley function, by not exposing the accumulators
-- If the algorithm were to be distributed as a library, only this function would be exposed
galeShapley :: Int -> Map Int [Int] -> Map Int (Map Int Int) -> (Map Int Int, Int)
galeShapley n = doGaleShapley unmatchedHospitals rounds hospitalsMap studentsMap
  where
    unmatchedHospitals = [0 .. (n - 1)]
    rounds = 0
    hospitalsMap = empty
    studentsMap = empty

-- Creates a map for each student of hospitalId to value. This allows us to determine whether
-- a student prefers one hospital to another in constant time, rather than having to traverse
-- the preference list to determine which hospital appears first.
makeStudentValueMap :: Int -> [Int] -> Map Int Int
makeStudentValueMap n preferenceList = fromList (zip preferenceList [(n - 1), (n - 2) .. 0])

main = do
  (n, k, h, s, hi, si) <- readInput

  -- A map of hospitalId -> PreferenceList
  let hospitals = fromList (map (\q -> (q, h !! (hi !! q))) [0 .. (n - 1)])

  -- A map of studentId -> a map of hospitalId -> the rank of that hospital to the student
  -- This is used instead of a preference list so we can look up the rank of a hospital to a student
  -- in constant time, and therefore determine whether or not a student will accept or reject
  -- a new offer in O(1)
  let students = fromList (map (\q -> (q, makeStudentValueMap n (s !! (si !! q)))) [0 .. (n - 1)])

  let (hospitalsMap, rounds) = galeShapley n hospitals students
  print rounds
  mapM_ (uncurry (printf "%d %d\n")) (toList hospitalsMap)

-- !1 All functions in Haskell are curried. The type declaration you see above each function
--    declaration shows each argument's type separated by an arrow. The last type is the return
--    type.
--
-- !2 Unlike many languages like Python and Javascript, Haskell has no Null data type. Instead
--    you can use the Maybe type, which contains either `Nothing`, or `Just a`, where a is a value.
--    You can then pattern match on the Maybe to handle both of those cases.
--
--    Unlike languages with null, Haskell's compiler forces you to handle both cases. You can't
--    forget that `Nothing` is a possibility, which is a common pitfall in many nullable languages.
--    This means less potential for runtime errors.
