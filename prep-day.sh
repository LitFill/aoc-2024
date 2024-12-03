#!/usr/bin/env bash

set -euxo pipefail

YEAR=2024
SESSION_FILE=.session

if [ -z "$1" ]; then
	echo "Must provide a day of the month as the first argument."
	exit 1
fi

DAY=$(echo "$1" | bc)
if [[ $DAY -lt 1 || $DAY -gt 25 ]]; then
	echo "The day must be between 1 and 25, inclusive."
	exit 1
fi

if [ ! -f "$SESSION_FILE" ]; then
	echo "File '$SESSION_FILE' with the user's session key from the Advent of" \
		"Code website does not exist."
	exit 1
fi

SESSION="$(cat "$SESSION_FILE")"
if [ -z "$SESSION" ]; then
	echo "Must set the session from the Advent of Code website."
	exit 1
fi

DAY_DIR="$DAY"
if [[ $DAY -ge 1 && $DAY -le 9 ]]; then
	DAY_DIR=0$DAY
fi

if [[ -d $DAY_DIR ]]; then
	echo "direktori untuk hari $DAY sudah ada, melewatinya..."
else
	mkdir -p $DAY_DIR
fi

DIR_MODULE="$DAY_DIR/Main.hs"
if [ -f "$DIR_MODULE" ]; then
	echo "'$DIR_MODULE' already exists, skipping..."
else
	cat <<-EOF >"$DIR_MODULE"
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
	EOF
fi

INPUT_FILE="$DAY_DIR/input.txt"
if [ -f "$INPUT_FILE" ]; then
	echo "File input untuk hari $DAY sudah ada, melewati..."
else
	echo "Downloading input data for day $DAY to '$INPUT_FILE'..."
	curl "https://adventofcode.com/$YEAR/day/$DAY/input" -s -m 10 \
		-b "session=$SESSION" >"$INPUT_FILE"
fi

SAMPLE_FILE="$DAY_DIR/samle.txt"
if [ -f "$SAMPLE_FILE" ]; then
	echo "File sample untuk hari $DAY sudah ada, melewati..."
else
	touch "$SAMPLE_FILE"
fi

echo "selamat memulai!"
