#! /bin/sh

echo "digraph G
{"
find "$1" -name \*.class \
  | while read -r x; do
    javap -v "$x" | grep " = Class" | sed "s%.*// *%\"$x\" -> %" | sed "s/$1\///" | sed "s/-> \(.*\)$/-> \"\1\"/"
  done
echo "}"
