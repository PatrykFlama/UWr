#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Usage: $0 <folder_name>"
    exit 1
fi

mkdir -p "$1"
cd "$1" || exit 1


for element in ../freertos_atmega328P/*; do
    ln -s "$element" .
done

rm main.c
cp ../freertos_atmega328P/main.c .
