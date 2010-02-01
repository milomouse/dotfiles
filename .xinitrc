## ~/.xinitrc
xmodmap ~/.Xmodmap &
#eval `cat ~/.fehbg` &
Esetroot -scale ~/foto/wall/200906125407-15574.jpg &
#if [ -x /usr/bin/display ]; then
#   dir=$HOME/foto/wall/shuf
#   get=$(find $dir/*|grep -ie '.jpg\|.png'|sort -R|tail -1)
#   display -window root -resize 1366x768! $get &
#fi
#exec musca >> /dev/null
exec ratpoison >> /dev/null
