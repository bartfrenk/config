#!/bin/bash
for f in $(find . -name "*.hs" -type f); do
  count=$(grep $1 $f | wc -l)
  if [ $count -gt 0 ]; then
    echo $count $f;
  fi
done | grep -v Test | sort -gr
