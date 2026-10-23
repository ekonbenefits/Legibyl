#!/usr/bin/env bash
# Runs a test project (already built), and fails if fewer of its tests ran than it has: a run
# that stops part of the way can still report "Passed!" (see #233). If the test process crashes,
# it names the tests that had started and not finished (from --blame): one of them crashed it.
set -euo pipefail
project="$1"
name="$(basename "$project")"
results="TestResults/$name"
expected=$(dotnet test "$project" --no-build --list-tests | grep -c '^    ')
status=0
dotnet test "$project" --no-build --blame --logger "trx;LogFileName=$name.trx" --results-directory "$results" || status=$?
unfinished=$(find "$results" -name 'Sequence_*.xml' -exec grep -h 'Completed="False"' {} + \
    | sed -e 's/.* Name="\([^"]*\)".*/\1/' -e "s/&apos;/'/g" -e 's/&quot;/"/g' -e 's/&lt;/</g' -e 's/&gt;/>/g' -e 's/&amp;/\&/g' | awk '!seen[$0]++' || true)
if [ -n "$unfinished" ]; then
    echo "::error title=$name::The test process stopped with these tests unfinished:%0A${unfinished//$'\n'/%0A}"
fi
ran=$(grep -o 'total="[0-9]*"' "$results/$name.trx" | head -1 | tr -dc '0-9' || true)
if [ "${ran:-0}" -lt "$expected" ]; then
    echo "::error title=$name::Only ${ran:-0} of its $expected tests ran."
    exit 1
fi
if [ "$status" -ne 0 ]; then
    exit "$status"
fi
echo "$name: all $ran of its $expected tests ran."
