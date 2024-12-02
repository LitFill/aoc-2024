module Main where

import Data.Functor ((<&>))
import Data.List.Extra (takeEnd)
import Data.Tuple.Extra (both, dupe, (&&&), (***))
import Flow

isIncreasingOrDecresing, isIncreasingBy1to3, isSafe :: [Int] -> Bool
isSafe =
  (isIncreasingOrDecresing &&& isIncreasingBy1to3)
    .> uncurry (&&)
isIncreasingOrDecresing =
  slidingBy 2
    .> (map (\[a, b] -> a < b) &&& map (\[a, b] -> a > b))
    .> both and
    .> uncurry (||)
isIncreasingBy1to3 =
  slidingBy 2
    .> map (\[a, b] -> dist a b)
    .> all (`elem` [1, 2, 3])

slidingBy :: Int -> [a] -> [[a]]
slidingBy n xs
  | length xs < n = []
  | otherwise = take n xs : slidingBy n (tail xs)

dist :: Int -> Int -> Int
dist a b = abs $ a - b

parseInput :: String -> [[Int]]
parseInput str =
  lines str
    <&> words
      .> map (read @Int)

drop1At n xs
  | n >= length xs = xs
  | otherwise = take n xs <> takeEnd (length xs - n - 1) xs

dropUnsafe = aux 0
 where
  aux n xs
    | n > length xs = []
    | otherwise = drop1At n xs : aux (n + 1) xs

part1 =
  parseInput
    .> map isSafe
    .> filter id
    .> length

part2 =
  parseInput
    .> map (dropUnsafe .> any isSafe)
    .> filter id
    .> length

main :: IO ()
main = do
  input <- readFile "input.txt"
  -- \| jawaban part 1
  putStr "Part 1: "
  print $ part1 input
  -- \| jawaban part 2
  putStr "Part 2: "
  print $ part2 input
