#!/bin/sh
## ~/.xinitrc
fi
if [ -f ~/.Xmodmap ]; then
   xmodmap ~/.Xmodmap
fi
if [ -x /usr/bin/display ]; then
   dir=~/foto/wall/shuf
   get=$(find $dir/*|grep -ie '.jpg\|.png' | sort -R | tail -1)
   display -window root -resize 1366x768! $get
fi
#exec musca >> /dev/null
#exec subtle >> /dev/null
exec ratpoison >> /dev/null
