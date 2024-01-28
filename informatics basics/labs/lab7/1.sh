#!/bin/bash

[[ $# = 2 ]] || { echo "usage: ${0} <delay> <command>"; exit 0; }

cmd="${2}"
[[ -x $cmd ]] || { echo "error: can't execute $cmd"; exit 1; }

filename=$(basename $cmd)
filename="${filename%.*}"
errfile="$filename-err.txt"
outfile="$filename-out.txt"

while true; do
  ( [[ `ps ax | grep -v grep | grep -v "${0}" | grep $cmd | wc -l` -gt 0 ]] && 
    { echo "Program is still running... Can't restart now"; } ) ||
    { echo "Restarting program..."; ./$cmd 2&>> $errfile 1&>> $outfile & }
  sleep $1
done
