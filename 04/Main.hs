module Main where

import Data.Tuple.Extra (both, (***))
import Flow

type Grid = [[Char]]
type Position = (Int, Int)
type Direction = (Int, Int)
type Length = Int

getSubstring :: Grid -> Position -> Direction -> Length -> String
getSubstring grid (x, y) (dx, dy) len =
  [ grid !! (x + i * dx) !! (y + i * dy)
  | i <- [0 .. len - 1]
  , x + i * dx >= 0 && x + i * dx < length grid
  , y + i * dy >= 0 && y + i * dy < length (head grid)
  ]

directions :: [Direction]
directions =
  [ (x, y)
  | x <- [-1 .. 1]
  , y <- [-1 .. 1]
  , not (x == 0 && y == 0)
  ]

checkAllDirections :: Grid -> Position -> String -> Int
checkAllDirections grid pos target =
  length
    <| filter (== target)
    <| [ getSubstring grid pos dir (length target)
       | dir <- directions
       ]

countTarget :: String -> Grid -> Int
countTarget target grid =
  sum
    [ checkAllDirections grid (x, y) target
    | x <- [0 .. length grid - 1]
    , y <- [0 .. length (head grid) - 1]
    ]

checkA :: Grid -> Position -> Int
checkA grid (ax, ay) =
  length
    <| filter (== target2)
    <| [ getSubstring grid pos dir 3
       | pos <- poses
       , dir <- [both negate (pos |> ((+ negate ax) *** (+ negate ay)))]
       ]
 where
  poses =
    [ (ax - 1, ay - 1)
    , (ax - 1, ay + 1)
    , (ax + 1, ay - 1)
    , (ax + 1, ay + 1)
    ]

checkX'MAS :: Grid -> Int
checkX'MAS grid =
  posOfAs grid
    |> map (checkA grid)
    |> filter (> 1)
    |> length

posOfAs :: Grid -> [Position]
posOfAs grid =
  zip (concat grid) [(x, y) | x <- [0 .. length grid - 1], y <- [0 .. length (head grid) - 1]]
    |> filter (fst .> (== 'A'))
    |> map snd

target1, target2 :: String
target1 = "XMAS"
target2 = "MAS"

parseInput :: String -> Grid
parseInput = lines

part1, part2 :: String -> Int
part1 = parseInput .> countTarget target1
part2 = parseInput .> checkX'MAS

main :: IO ()
main = do
  input <- readFile "input.txt"
  -- \| jawaban part 1
  putStr "Part 1: "
  print <| part1 input
  -- \| jawaban part 2
  putStr "Part 2: "
  print <| part2 input
