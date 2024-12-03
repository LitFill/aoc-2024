module Main where

import Data.List.Extra (drop1, stripPrefix)
import Data.Tuple.Extra ((&&&))
import Flow
import Text.Read (readMaybe)
import Text.Regex.TDFA (getAllTextMatches, (=~))

parseInput :: String -> String -> [String]
parseInput regex =
  (=~ regex)
    .> getAllTextMatches

regex1 = "mul\\([0-9]+,[0-9]+\\)"
regex2 = "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"

parseMult :: String -> Int
parseMult =
  stripPrefix "mul"
    .> maybe Nothing readMaybe
    .> maybe 0 (uncurry (*))

select =
  takeWhile (/= "don't()")
    &&& dropWhile (/= "don't()")
    .> dropWhile (/= "do()")
    .> drop1

repeatedly :: ([a] -> ([b], [a])) -> [a] -> [b]
repeatedly f [] = []
repeatedly f as = b <> repeatedly f as'
 where
  (b, as') = f as

part1 :: String -> Int
part1 =
  parseInput regex1
    .> map parseMult
    .> sum

part2 :: String -> Int
part2 =
  parseInput regex2
    .> repeatedly select
    .> map parseMult
    .> sum

main :: IO ()
main = do
  input <- readFile "input.txt"
  -- \| jawaban part 1
  putStr "Part 1: "
  print $ part1 input
  -- \| jawaban part 2
  putStr "Part 2: "
  print $ part2 input
