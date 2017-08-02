#!/bin/bash

# Write files to current directory
FOLDER="$(pwd)"

# Create a temporary file
ftfile="$(mktemp -p "$FOLDER")"

# Encode STDIN to temporary file
cat | vaportrail enc > "$ftfile"

# Transmit file. Use taskset to avoid hopping between cores, which should
# help with timing a bit
taskset -c 0 sudo rpitx -m RF -f 97700 -i "$ftfile"

# Remove temporary file
rm "$ftfile"
