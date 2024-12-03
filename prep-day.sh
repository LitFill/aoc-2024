#!/usr/bin/env bash

set -euxo pipefail

YEAR=2024
SESSION_FILE=".session"

# Fungsi untuk memeriksa file/direktori
ensure_file() {
	local file_path="$1"
	local default_content="$2"

	if [ -f "$file_path" ]; then
		echo "File '$file_path' sudah ada, melewati..."
	else
		echo "Membuat file '$file_path'..."
		echo -n "$default_content" >"$file_path"
	fi
}

ensure_directory() {
	local dir_path="$1"
	if [ -d "$dir_path" ]; then
		echo "Direktori '$dir_path' sudah ada, melewati..."
	else
		echo "Membuat direktori '$dir_path'..."
		mkdir -p "$dir_path"
	fi
}

# Validasi argumen hari
if [ -z "${1:-}" ]; then
	echo "Harus menyertakan hari dalam argumen pertama."
	exit 1
fi

DAY=$(printf "%d" "$1")
if [[ $DAY -lt 1 || $DAY -gt 25 ]]; then
	echo "Hari harus antara 1 hingga 25, inklusif."
	exit 1
fi

# Validasi file sesi
if [ ! -f "$SESSION_FILE" ]; then
	echo "File '$SESSION_FILE' tidak ditemukan."
	exit 1
fi

SESSION=$(<"$SESSION_FILE")
if [ -z "$SESSION" ]; then
	echo "Sesi tidak valid, pastikan sudah diatur dari Advent of Code."
	exit 1
fi

# Tentukan nama direktori
DAY_DIR=$(printf "%02d" "$DAY")
ensure_directory "$DAY_DIR"

# Buat modul utama jika belum ada
DIR_MODULE="$DAY_DIR/Main.hs"
if [ ! -f "$DIR_MODULE" ]; then
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
else
	echo "Modul utama '$DIR_MODULE' sudah ada, melewati..."
fi

# Unduh input data jika belum ada
INPUT_FILE="$DAY_DIR/input.txt"
if [ ! -f "$INPUT_FILE" ]; then
	echo "Mengunduh input untuk hari $DAY ke '$INPUT_FILE'..."
	curl -fsSL --max-time 10 -b "session=$SESSION" \
		"https://adventofcode.com/$YEAR/day/$DAY/input" >"$INPUT_FILE" || {
		echo "Gagal mengunduh input, pastikan sesi dan koneksi benar."
		exit 1
	}
else
	echo "File input '$INPUT_FILE' sudah ada, melewati..."
fi

# Buat file sample jika belum ada
SAMPLE_FILE="$DAY_DIR/sample.txt"
ensure_file "$SAMPLE_FILE" ""

echo "Persiapan untuk hari $DAY selesai! Selamat memulai!"
