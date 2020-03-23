#!/usr/bin/env bash
for file in "$@"
do
  bat --style=full --paging=always "$file"
done
