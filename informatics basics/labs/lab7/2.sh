#!/bin/bash

rec() {
  local result=0
  if [ -d "$1" ]; then
    while read name; do
      [[ $name != "" ]] && { result=$(($result+`rec "$1/$name"`)); }
    done <<< $(ls "$1")
    echo $result
  else
    name=$(basename "$1")
    ext="${name##*.}"
    if [ "$ext" = "c" ] || [ "$ext" = "h" ]; then
      echo `cat "$1" | sed '/^\s*$/d' | wc -l`
    else
      echo 0
    fi
  fi
}

[[ "$1" = "" ]] && rec "$PWD" || rec "$1"
