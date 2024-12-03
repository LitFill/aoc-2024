#!/usr/bin/env sh
set -euxo pipefail

out=$(basename $PWD)
binname="day$out"

build() {
	ghc ./Main -Werror -O2 -o $binname
}

clean() {
	[ -f "./Main.hi" ] && rm -f ./Main.hi
	[ -f "./Main.o" ] && rm -f ./Main.o
}

run() {
	[ -f $binname ] && ./$binname
}

main() {
	if [[ ! -f "Main.hs" ]]; then
		echo "ERROR: file Main.hs tidak ditemukan di direktori ini." >&2
		exit 69
	fi

	build
	clean
	run
}
main
