#!/bin/sh
# Print a message.
if [ -n "$1" ]; then
  echo "Hello, $1"
else
  echo "Hello, world"
fi
cat <<END
A heredoc line.
END
