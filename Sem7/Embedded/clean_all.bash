#!/usr/bin/env bash

# remove all .hex apart from arduinoisp.hex
find . -type f -name "*.hex" ! -name "arduinoisp.hex" -exec rm {} \;

# iterate over extensions and remove files
for ext in o eps png bak bin srec lst map .bootloader-pad elf; do
    find . -type f -name "*.${ext}" -exec rm {} \;
done
