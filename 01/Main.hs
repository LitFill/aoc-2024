module Main where

import Data.Functor ((<&>))
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Tuple.Extra (both, second)
import Flow

vec2ToTuple :: [b] -> (b, b)
vec2ToTuple [a, b] = (a, b)

parseInput :: String -> [(Int, Int)]
parseInput str =
  lines str
    <&> words
      .> vec2ToTuple
      .> both read

part1 :: String -> Int
part1 =
  parseInput
    .> unzip
    .> both sort
    .> uncurry zip
    .> map (uncurry dist)
    .> sum

dist :: Int -> Int -> Int
dist a b = abs $ a - b

part2 =
  parseInput
    .> unzip
    .> second mapFromList
    .> weightedSum

weightedSum :: (Ord b, Num b) => ([b], Map b b) -> b
weightedSum (xs, m) =
  xs
    |> map (\x -> Map.findWithDefault 0 x m * x)
    |> sum

mapFromList :: (Ord k, Num v) => [k] -> Map k v
mapFromList = foldl insert Map.empty

insert :: (Ord k, Num a) => Map k a -> k -> Map k a
insert acc k = Map.insertWith (+) k 1 acc

main :: IO ()
main = do
  input <- readFile "input.txt"
  -- \| jawaban part 1
  putStr "Part 1: "
  print $ part1 input
  -- \| jawaban part 2
  putStr "Part 2: "
  print $ part2 input
