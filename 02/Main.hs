module Main where

import Data.List.Extra (tails)
import Data.Tuple.Extra ((&&&))
import Flow

isSafe :: [Int] -> Bool
isSafe =
  (isIncreasingOrDecresing &&& isIncreasingBy1to3)
    .> uncurry (&&)

isIncreasingOrDecresing :: [Int] -> Bool
isIncreasingOrDecresing =
  slidingBy 2
    .> map vec2ToTuple
    .> (all (uncurry (<)) &&& all (uncurry (>)))
    .> uncurry (||)

isIncreasingBy1to3 :: [Int] -> Bool
isIncreasingBy1to3 =
  slidingBy 2
    .> all
      ( vec2ToTuple
          .> uncurry dist
          .> (`elem` [1, 2, 3])
      )

slidingBy :: Int -> [a] -> [[a]]
slidingBy n =
  tails
    .> map (take n)
    .> filter (length .> (== n))

dist :: Int -> Int -> Int
dist a b = abs $ a - b

dist' = (abs .) . (-)
dist'' = (-) .> (.> abs)

parseInput :: String -> [[Int]]
parseInput =
  lines
    .> map (words .> map read)

drop1At :: Int -> [a] -> [a]
drop1At n xs =
  let (l, _ : r) = splitAt n xs
   in l <> r

dropUnsafe :: [a] -> [[a]]
dropUnsafe xs =
  [ drop1At i xs
  | i <- [0 .. length xs - 1]
  ]

vec2ToTuple :: [b] -> (b, b)
vec2ToTuple [a, b] = (a, b)

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
