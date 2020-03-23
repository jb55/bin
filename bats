#!/usr/bin/env bash
REMOVES=()

for file in "$@"
do
  base="$(basename "$file")"
  dir="$(dirname "$file")"
  colorfile="$dir/.${base}.color"
  bat --style=full --paging=never --color=always "$file" > "$colorfile"
  REMOVES+=("$colorfile")
done

"$PAGER" "${REMOVES[@]}"

rm -f "${REMOVES[@]}"
