module Main where

import Flow

part1, part2 :: String -> Int
part1 = undefined
part2 = undefined

main :: IO ()
main = do
  input <- readFile "input.txt"
  -- \| jawaban part 1
  putStr "Part 1: "
  print <| part1 input
  -- \| jawaban part 2
  putStr "Part 2: "
  print <| part2 input
