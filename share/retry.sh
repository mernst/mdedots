#!/bin/sh
# Source this file to define `retry`, which runs a command and retries it
# twice if it fails, as GitHub's GraphQL API sometimes does.  Each attempt's
# output is buffered, so a failed attempt produces no output.

retry() {
  retry_attempt=1
  until retry_output=$("$@"); do
    if [ "$retry_attempt" -eq 3 ]; then
      return 1
    fi
    sleep $((retry_attempt * 30))
    retry_attempt=$((retry_attempt + 1))
  done
  printf '%s\n' "$retry_output"
}
