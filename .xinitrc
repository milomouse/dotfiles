#!/bin/sh
## ~/.xinitrc
xmodmap ~/.Xmodmap &
display -window root ~/foto/wall/black_mosaic_by_seikq.jpg &
#if [ -x /usr/bin/display ]; then
#   dir=$HOME/foto/wall/shuf
#   get=$(find $dir/*|grep -ie '.jpg\|.png'|sort -R|tail -1)
#   display -window root -resize 1366x768! $get &
#fi
#exec musca >> /dev/null
exec ratpoison >> /dev/null
