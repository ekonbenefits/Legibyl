#!/usr/bin/env bash
# Runs a test project (already built), and fails if fewer of its tests ran than it has: a run
# that stops part of the way can still report "Passed!" (see #233).
set -euo pipefail
project="$1"
name="$(basename "$project")"
expected=$(dotnet test "$project" --no-build --list-tests | grep -c '^    ')
dotnet test "$project" --no-build --logger "trx;LogFileName=$name.trx" --results-directory TestResults
ran=$(grep -o 'total="[0-9]*"' "TestResults/$name.trx" | head -1 | tr -dc '0-9')
if [ "${ran:-0}" -lt "$expected" ]; then
    echo "::error title=$name::Only $ran of its $expected tests ran."
    exit 1
fi
echo "$name: all $ran of its $expected tests ran."
