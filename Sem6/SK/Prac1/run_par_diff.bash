#!/bin/bash

if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <program> <arg1> [<arg2> ... <argN>]"
    exit 1
fi

PROGRAM=$1
shift

mkdir -p ./results_run_parallel

for ARG in "$@"; do
    eval $PROGRAM $ARG > ./results_run_parallel/run_$ARG &
done

# wait for all background processes to finish
wait

echo "All processes have completed."
