#!/bin/bash

# Create a temporary file in current directory
FOLDER="$(pwd)"
rawfile="$(mktemp -p "$FOLDER")"

# Listen on UDP port 7355 (default GQRX port) and write to file in background
# Listen on both IPv4 and IPv6 ports, as sometimes GQRX likes to send to one,
# and sometimes it sends to the other.
socat -u 'UDP4-RECV:7355,bind=localhost' 'STDOUT' > "$rawfile" &
scjob1=$!
socat -u 'UDP6-RECV:7355,bind=localhost' 'UDP4-SEND:localhost:7355' &
scjob2=$!

# Wait for user to press enter. When they do, kill the socat job
read -p "Press enter when done transmitting."
kill $scjob1 $scjob2
wait $scjob1 $scjob2

# Inform the user we're decoding, print that to stderr
echo "Decoding..." 1>&2
echo "" 1>&2

# Decode from raw file, output goes to stdout
vaportrail dec < "$rawfile"

# Remove temporary file
rm "$rawfile"
