#!/bin/bash

function process(){
  local DEBUG=0
  local file_name="$(basename -s .JPG "$1")"
  local rotate_out="rot-$file_name.png"
  local unpaper_in="$file_name.pbm"
  local dewarped="${rotate_out%.png}_thresh.png"
  local unpaper_out="out-${unpaper_in}"
  local trimed_out="trimed-${file_name}.png"
  local bezNumeru="bezNumeru-${trimed_out}"
  local marginesy="marginesy-${file_name}.png"

  magick "$file_name.JPG" -rotate 90 "$rotate_out"
  #page-dewarp -x 10 -y 15 -wz 97 -f 3 -tk 20 "$rotate_out"
  page-dewarp -x 10 -y 15 -wz 157 -f 5 -tk 20 "$rotate_out"
  convert "$dewarped" "$unpaper_in"
  #unpaper --no-deskew -bs 10,10 -bd 100 -bi 35 -l none "$unpaper_in" "$unpaper_out"
  unpaper --no-deskew -bs 10 -bd 100 -bp 3 -bi 35 "$unpaper_in" "$unpaper_out"
  #magick "$unpaper_out" \( +clone -blur 0x20 -fuzz 15% -trim -set option:fuzzy_trim '%[fx:w+20]x%[fx:h+20]+%[fx:page.x-10]+%[fx:page.y-10]' +delete \) -crop %[fuzzy_trim] -colorspace Gray +repage "$trimed_out"
  magick "$unpaper_out" \( -background white -virtual-pixel white +clone -blur 0x30 -fuzz 15% -trim -set option:fuzzy_trim '%[fx:w+20]x%[fx:h+20]+%[fx:page.x-10]+%[fx:page.y-10]' +delete \) -crop %[fuzzy_trim] -colorspace Gray +repage "$trimed_out"
  ../usun_numer_strony.py "$trimed_out"
  magick "$bezNumeru" -background white -resize 2800x4480\! -splice 550x400 -gravity southeast -splice 550x400 +repage "${marginesy}"

  if [ $DEBUG -eq 0 ] ; then
    rm "$dewarped" "$unpaper_in" "$unpaper_out" "$trimed_out" "$bezNumeru" "$rotate_out"
  fi

}
export -f process

#parallel -j 12 process {} ::: *.JPG
parallel -j 12 process {} ::: IMG_0460.JPG
