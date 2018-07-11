#!/bin/bash
# Proper header for a Bash script.

# Cleanup., version 2

# Run as root, of course.
# Inser code here to print error message and exit if not root.

LOG_DIR=/var/log
# Variables are etter than hard-coded values.
cd $LOG_DIR

cat /dev/null > messages
cat /dev/null > wtmp

echo "Logs cleaned up."

exit # The right and proper method pf "existing" from a script.
# A bare "exit" (no parameter) returns exit status
#+ of the precening command.
