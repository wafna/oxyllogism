#!/usr/bin/env bash

# Trims whitespace off the ends off all the lines in the source files.
# Idempotent.
for FILE in $(find src/ -name *.hs)
do
    echo "$FILE"
    sed -i 's/[[:space:]]*$//' "$FILE"
done