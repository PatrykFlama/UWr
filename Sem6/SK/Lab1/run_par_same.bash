#!/bin/bash

if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <number_of_times> <program> [<arguments>...]"
    exit 1
fi

NUM_TIMES=$1
PROGRAM=$2
shift 2

mkdir -p ./results_run_parallel
mkdir -p ./results_traceroute
mkdir -p ./results_ping

for ((i=0; i<NUM_TIMES; i++)); do
    $PROGRAM "$@" > ./results_run_parallel/run$i &
    traceroute -I "$@" > ./results_traceroute/traceroute$i &
    ping -c 5 "$@" > ./results_ping/ping$i &
done

# wait for all background processes to finish
wait

echo "All processes have completed."
