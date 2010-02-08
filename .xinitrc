#!/bin/sh
## ~/.xinitrc
xmodmap ~/.Xmodmap &
#if [ -x /usr/bin/display ]; then
#   dir=$HOME/foto/wall/shuf
#   get=$(find $dir/*|grep -ie '.jpg\|.png'|sort -R|tail -1)
#   display -window root -resize 1366x768! $get
#fi
display -window root ~/foto/wall/black_mosaic_by_seikq.jpg &
exec ratpoison >> /dev/null
