#!/bin/bash

LOG_FILE="/var/log/mystat.log"
OUTPUT_DIR="/var/lib/mystat"
DATA_FILE="${OUTPUT_DIR}/cpu_usage.dat"
PNG_FILE="${OUTPUT_DIR}/cpu_usage_$(date +\%Y\%m\%d).png"

mkdir -p ${OUTPUT_DIR}

# extract data from logs ([YYYY-MM-DD HH:mm:ss] CPU Usage: Avg=12.58%, Min=0.25%, Max=44.68%)
awk '
    /CPU Usage:/ {
        # combine date and tiem (fields $1 and $2)
        gsub(/[\[\]]/, "", $1); 
        gsub(/[\[\]]/, "", $2);
        timestamp = $1 " " $2;
        
        # extract Avg from field $5 ("Avg=12.58%,")
        split($5, arr, "=");
        gsub(/%,/, "", arr[2]);
        avg = arr[2];
        
        print timestamp " " avg
    }
' ${LOG_FILE} > ${DATA_FILE}


# calc ranges
Y_MIN=$(awk 'NR == 1 {min = $3} $3 < min {min = $3} END {print min-1}' ${DATA_FILE})
Y_MAX=$(awk 'NR == 1 {max = $3} $3 > max {max = $3} END {print max+1}' ${DATA_FILE})

# plot
gnuplot <<- EOF
    set terminal pngcairo size 800,600
    set output "${PNG_FILE}"
    set xdata time
    set timefmt "%Y-%m-%d %H:%M:%S"
    set format x "%H:%M"
    set xlabel "Time"
    set ylabel "Load CPU (%)"
    set title "Load CPU during last 24h"
    set yrange [${Y_MIN}:${Y_MAX}]
    set grid
    set key top left
    plot "${DATA_FILE}" using 1:3 with lines lw 2 title "Avg load"
EOF


rm ${DATA_FILE}