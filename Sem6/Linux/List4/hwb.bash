#!/bin/bash

# default options
greeting="Hello"
capitalize=false
color="auto"
print_world=false
version="hwb 1.0"

copyright="
Copyright (C) 2025 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Written by me.
"

# is stdout terminal
# https://unix.stackexchange.com/questions/401934/how-do-i-check-whether-my-shell-is-running-in-a-terminal
is_terminal() {
    tty -s
}

# colorizing
colorize() {
    local text="$1"
    if [[ "$color" == "always" || ( "$color" == "auto" && is_terminal ) ]]; then
        echo -e "\e[1;34m$text\e[0m"  # blue clor
    else
        echo "$text"
    fi
}

# agruments parsing
ARGS=$(
    getopt -o cg:whv -l capitalize,world,greeting:,color:,help,version -- "$@"
    ) || exit 1
eval set -- "$ARGS"

# arguments handling
while true; do
    case "$1" in
        -c|--capitalize)
            capitalize=true
            shift ;;
        -g|--greeting)
            greeting="$2"
            shift 2 ;;
        --color)
            color="$2"
            shift 2 ;;
        -w|--world)
            print_world=true
            shift ;;
        -h|--help)
            echo "usage: hwb [OPTIONS] [NAMES...]"
            echo "print greetings"
            echo 
            echo "options:"
            echo "  -c, --capitalize            capitalize the name or 'world'"
            echo "  --color=MODE                set color mode (never, auto, always)"
            echo "  -g TEXT, --greeting=TEXT    set greeting text instead of 'Hello'"
            echo "  -w, --world                 also print 'Hello, world!'"
            echo "  -h, --help                  display this help message"
            echo "  -v, --version               display program version"
            exit 0 ;;
        -v|--version)
            echo "$version"
            echo "$copyright"
            exit 0 ;;
        --)
            shift
            break ;;
    esac
done

# write greeting
greet() {
    local name="$1"
    if [[ "$capitalize" == true ]]; then
        name="$(echo "$name" | awk '{print toupper(substr($0,1,1)) tolower(substr($0,2))}')"
    fi
    echo "$(colorize "$greeting, $name!")"
}

# default greeting
if [[ $# -eq 0 && "$print_world" == false ]]; then
    greet "world"
fi

# print greetings for each name
for name in "$@"; do
    greet "$name"
done

# print hw
if [[ "$print_world" == true ]]; then
    greet "world"
fi
