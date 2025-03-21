#!/bin/bash

if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <program> <arg1> [<arg2> ... <argN>]"
    exit 1
fi

PROGRAM=$1
shift

mkdir -p ./results_run_parallel
mkdir -p ./results_traceroute
mkdir -p ./results_ping

for ARG in "$@"; do
    eval $PROGRAM $ARG > ./results_run_parallel/run_$ARG &
    eval traceroute -I $ARG > ./results_traceroute/traceroute_$ARG &
    eval ping -c 5 $ARG > ./results_ping/ping_$ARG &
done

# wait for all background processes to finish
wait

echo "All processes have completed."
