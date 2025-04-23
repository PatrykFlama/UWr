#!/bin/bash

LOG_FILE="/var/log/mystat.log"
OUTPUT_DIR="/var/lib/mystat"
DATA_FILE="${OUTPUT_DIR}/cpu_usage.dat"
PNG_FILE="${OUTPUT_DIR}/cpu_usage_$(date +\%Y\%m\%d).png"

# Utwórz katalog jeśli nie istnieje
mkdir -p ${OUTPUT_DIR}

# Przetwórz logi na format dla gnuplota
awk '
    /CPU Usage:/ {
        gsub(/[\[\]]/, "", $2); 
        gsub(/%/, "", $4); 
        print $2 " " $4
    }
' ${LOG_FILE} > ${DATA_FILE}

# Generuj wykres z użyciem gnuplota
gnuplot <<- EOF
    set terminal pngcairo size 800,600
    set output "${PNG_FILE}"
    set xdata time
    set timefmt "%Y-%m-%d %H:%M:%S"
    set format x "%H:%M"
    set xlabel "Czas"
    set ylabel "Obciazenie CPU (%)"
    set title "Obciazenie CPU w ciagu ostatnich 24h"
    plot "${DATA_FILE}" using 1:3 with lines title "Srednie obciazenie"
EOF

# Oczyść dane tymczasowe
rm ${DATA_FILE}