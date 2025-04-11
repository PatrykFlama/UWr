#!/bin/bash

if [ -z "$1" ]; then
    echo "Usage: $0 <number>"
    exit 1
fi

(cat "vm$1.in" ; cat) | ./router
